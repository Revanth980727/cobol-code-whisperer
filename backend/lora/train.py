
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
    parser.add_argument("--target_modules", type=str, default="q_proj,v_proj", help="Target modules for LoRA")
    parser.add_argument("--use_8bit", action="store_true", help="Use 8bit precision")
    
    return parser.parse_args()

def load_training_data(file_path):
    with open(file_path, "r") as f:
        data = json.load(f)
    
    # Extract prompts and responses for training
    train_data = {
        "prompt": [example["prompt"] for example in data],
        "completion": [example["completion"] for example in data],
    }
    
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
    
    model = AutoModelForCausalLM.from_pretrained(
        args.model_name,
        torch_dtype=torch.float16,
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
    else:
        train_data = tokenized_dataset
        val_data = None
    
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
        logging_steps=10,
        evaluation_strategy="epoch" if val_data else "no",
        save_strategy="epoch",
        load_best_model_at_end=True if val_data else False,
        report_to="none",  # Disable wandb
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
    
    logger.info("Training complete!")

if __name__ == "__main__":
    train()
