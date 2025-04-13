
import argparse
import json
import os
import torch
from datasets import Dataset
from transformers import (
    AutoModelForCausalLM,
    AutoTokenizer,
    TrainingArguments,
    Trainer,
    DataCollatorForLanguageModeling,
)
from peft import (
    get_peft_model,
    LoraConfig,
    TaskType,
    prepare_model_for_kbit_training,
)
import logging

# Set up logger
logging.basicConfig(
    format="%(asctime)s - %(levelname)s - %(name)s - %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
    level=logging.INFO,
)
logger = logging.getLogger("lora-training")

def parse_args():
    parser = argparse.ArgumentParser(description="Fine-tune a model with LoRA")
    parser.add_argument("--model_name", type=str, required=True, help="Name of the base model to use")
    parser.add_argument("--train_file", type=str, required=True, help="Path to training data JSON file")
    parser.add_argument("--output_dir", type=str, required=True, help="Directory to save the model")
    parser.add_argument("--batch_size", type=int, default=16, help="Batch size")
    parser.add_argument("--micro_batch_size", type=int, default=4, help="Micro batch size")
    parser.add_argument("--num_epochs", type=int, default=3, help="Number of training epochs")
    parser.add_argument("--learning_rate", type=float, default=3e-4, help="Learning rate")
    parser.add_argument("--cutoff_len", type=int, default=1024, help="Max token length")
    parser.add_argument("--val_set_size", type=int, default=0, help="Size of validation set")
    parser.add_argument("--lora_r", type=int, default=8, help="LoRA attention dimension")
    parser.add_argument("--lora_alpha", type=int, default=16, help="Alpha parameter for LoRA scaling")
    parser.add_argument("--lora_dropout", type=float, default=0.05, help="Dropout for LoRA layers")
    parser.add_argument("--target_modules", type=str, default="q_proj,v_proj", 
                        help="Target modules for LoRA (comma-separated)")
    parser.add_argument("--use_8bit", action="store_true", help="Use 8bit precision")
    parser.add_argument("--eval_steps", type=int, default=100, 
                        help="Steps between evaluations")
    parser.add_argument("--save_steps", type=int, default=100, 
                        help="Steps between checkpoint saves")
    parser.add_argument("--logging_steps", type=int, default=10, 
                        help="Steps between logging")
    parser.add_argument("--warmup_steps", type=int, default=100, 
                        help="Warmup steps for learning rate scheduler")
    parser.add_argument("--logging_dir", type=str, default=None, 
                        help="Directory for tensorboard logs")
    
    return parser.parse_args()

def load_training_data(file_path):
    with open(file_path, "r") as f:
        data = json.load(f)
    
    # Extract prompts and responses for training
    prompts = []
    completions = []
    
    for example in data:
        prompts.append(example.get("prompt", ""))
        completions.append(example.get("completion", ""))
    
    train_data = {
        "prompt": prompts,
        "completion": completions,
    }
    
    logger.info(f"Loaded {len(prompts)} training examples")
    return Dataset.from_dict(train_data)

def format_dataset(example, tokenizer, cutoff_len):
    prompt = example["prompt"]
    completion = example["completion"]
    
    # Format for instruction tuning
    full_text = f"<s>[INST] {prompt.strip()} [/INST] {completion.strip()} </s>"
    
    # Tokenize
    tokenized = tokenizer(
        full_text,
        truncation=True,
        max_length=cutoff_len,
        padding="max_length",
    )
    
    # Create labels (same as input_ids for causal LM)
    tokenized["labels"] = tokenized["input_ids"].copy()
    
    return tokenized

def train():
    args = parse_args()
    logger.info(f"Starting training with args: {args}")
    
    # Load the model and tokenizer
    logger.info(f"Loading model: {args.model_name}")
    tokenizer = AutoTokenizer.from_pretrained(args.model_name)
    
    # Add padding token if needed
    if tokenizer.pad_token is None:
        tokenizer.pad_token = tokenizer.eos_token
    
    # Load model with quantization if requested
    model_kwargs = {}
    if args.use_8bit:
        logger.info("Using 8-bit quantization")
        model_kwargs["load_in_8bit"] = True
    
    # Try different loading strategies
    try:
        logger.info("Trying to load model with torch_dtype=float16...")
        model = AutoModelForCausalLM.from_pretrained(
            args.model_name,
            torch_dtype=torch.float16,
            device_map="auto",
            **model_kwargs
        )
    except Exception as e:
        logger.warning(f"Failed to load with float16, trying again with defaults: {e}")
        model = AutoModelForCausalLM.from_pretrained(
            args.model_name,
            device_map="auto",
            **model_kwargs
        )
    
    # Prepare model for training
    if args.use_8bit:
        model = prepare_model_for_kbit_training(model)
    
    # Configure LoRA
    logger.info("Configuring LoRA")
    target_modules = args.target_modules.split(",")
    config = LoraConfig(
        r=args.lora_r,
        lora_alpha=args.lora_alpha,
        target_modules=target_modules,
        lora_dropout=args.lora_dropout,
        bias="none",
        task_type=TaskType.CAUSAL_LM,
    )
    model = get_peft_model(model, config)
    
    # Log trainable parameters
    logger.info("Trainable parameters:")
    trainable_params = 0
    all_params = 0
    for name, param in model.named_parameters():
        all_params += param.numel()
        if param.requires_grad:
            trainable_params += param.numel()
            logger.info(f"  {name}: {param.numel()}")
    logger.info(f"Trainable params: {trainable_params} ({trainable_params/all_params:.2%} of all params)")
    
    # Load and process the dataset
    logger.info(f"Loading training data from {args.train_file}")
    dataset = load_training_data(args.train_file)
    
    # Process the dataset
    def tokenize_function(example):
        return format_dataset(example, tokenizer, args.cutoff_len)
    
    tokenized_dataset = dataset.map(tokenize_function, batched=False)
    
    # Split into training and validation sets
    if args.val_set_size > 0:
        train_val = tokenized_dataset.train_test_split(
            test_size=args.val_set_size, shuffle=True, seed=42
        )
        train_data = train_val["train"]
        val_data = train_val["test"]
        logger.info(f"Training on {len(train_data)} examples, validating on {len(val_data)} examples")
    else:
        train_data = tokenized_dataset
        val_data = None
        logger.info(f"Training on {len(train_data)} examples, no validation set")
    
    # Set up training arguments
    gradient_accumulation_steps = max(1, args.batch_size // args.micro_batch_size)
    
    training_args = TrainingArguments(
        output_dir=args.output_dir,
        per_device_train_batch_size=args.micro_batch_size,
        per_device_eval_batch_size=args.micro_batch_size,
        gradient_accumulation_steps=gradient_accumulation_steps,
        num_train_epochs=args.num_epochs,
        learning_rate=args.learning_rate,
        fp16=not args.use_8bit,  # If using 8bit, don't use fp16
        logging_dir=args.logging_dir,
        logging_steps=args.logging_steps,
        evaluation_strategy="steps" if val_data else "no",
        eval_steps=args.eval_steps if val_data else None,
        save_strategy="steps",
        save_steps=args.save_steps,
        warmup_steps=args.warmup_steps,
        load_best_model_at_end=True if val_data else False,
        report_to="tensorboard" if args.logging_dir else "none",
    )
    
    # Create data collator for language modeling
    data_collator = DataCollatorForLanguageModeling(tokenizer=tokenizer, mlm=False)
    
    # Create trainer
    trainer = Trainer(
        model=model,
        args=training_args,
        train_dataset=train_data,
        eval_dataset=val_data,
        data_collator=data_collator,
    )
    
    # Start training
    logger.info("Starting training")
    trainer.train()
    
    # Save the model
    logger.info(f"Saving model to {args.output_dir}")
    model.save_pretrained(args.output_dir)
    tokenizer.save_pretrained(args.output_dir)
    
    # Also save the model adapter separately to make merging easier
    adapter_path = os.path.join(args.output_dir, "adapter")
    model.save_pretrained(adapter_path, adapter_name="cobol_adapter")
    
    # Save training arguments
    with open(os.path.join(args.output_dir, "training_args.json"), "w") as f:
        training_args_dict = {
            "model_name": args.model_name,
            "lora_r": args.lora_r,
            "lora_alpha": args.lora_alpha,
            "lora_dropout": args.lora_dropout,
            "target_modules": args.target_modules,
            "num_epochs": args.num_epochs,
            "learning_rate": args.learning_rate,
            "batch_size": args.batch_size,
            "micro_batch_size": args.micro_batch_size,
            "cutoff_len": args.cutoff_len,
            "val_set_size": args.val_set_size,
            "use_8bit": args.use_8bit,
            "trained_on": len(train_data),
        }
        json.dump(training_args_dict, f, indent=2)
    
    logger.info("Training complete!")

if __name__ == "__main__":
    train()
