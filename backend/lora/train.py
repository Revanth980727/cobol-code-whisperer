
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
    EvalPrediction
)
from peft import (
    get_peft_model,
    LoraConfig,
    TaskType,
    prepare_model_for_kbit_training,
)
import logging
import numpy as np
from sklearn.metrics import accuracy_score, f1_score

# Import our config handler
from backend.lora.config import load_config_from_args

# Set up logger
logging.basicConfig(
    format="%(asctime)s - %(levelname)s - %(name)s - %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
    level=logging.INFO,
)
logger = logging.getLogger("lora-training")

def load_training_data(file_path, validation_path=None):
    """Load training data and optional validation data"""
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
    
    # Load validation data if provided
    val_data = None
    if validation_path and os.path.exists(validation_path):
        with open(validation_path, "r") as f:
            val_data_raw = json.load(f)
            
        val_prompts = []
        val_completions = []
        
        for example in val_data_raw:
            val_prompts.append(example.get("prompt", ""))
            val_completions.append(example.get("completion", ""))
        
        val_data = {
            "prompt": val_prompts,
            "completion": val_completions,
        }
        logger.info(f"Loaded {len(val_prompts)} validation examples")
    
    return Dataset.from_dict(train_data), val_data

def format_dataset(example, tokenizer, cutoff_len):
    """Format dataset for instruction tuning"""
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

def compute_metrics(eval_pred):
    """Compute evaluation metrics for the model"""
    # This is a simplified metric function
    # In reality, evaluating text generation is more complex
    return {"accuracy": 0.5}  # Placeholder metric

def validate_model_on_holdout(model, tokenizer, holdout_file):
    """Validate fine-tuned model on holdout examples"""
    if not os.path.exists(holdout_file):
        logger.warning(f"Holdout file not found: {holdout_file}")
        return None
        
    # Load holdout validation data
    with open(holdout_file, "r") as f:
        holdout_data = json.load(f)
        
    results = []
    correct = 0
    total = 0
    
    # Process each validation example
    for example in holdout_data:
        prompt = example.get("prompt", "")
        reference = example.get("completion", "")
        
        # Generate prediction
        inputs = tokenizer(f"[INST] {prompt} [/INST]", return_tensors="pt")
        if torch.cuda.is_available():
            inputs = {k: v.to("cuda") for k, v in inputs.items()}
            
        with torch.no_grad():
            outputs = model.generate(
                **inputs,
                max_length=512,
                temperature=0.1,
                top_p=0.75,
                top_k=40,
                num_return_sequences=1
            )
        
        prediction = tokenizer.decode(outputs[0], skip_special_tokens=True)
        
        # Simple exact match metric
        is_correct = prediction.strip() == reference.strip()
        if is_correct:
            correct += 1
        total += 1
        
        results.append({
            "prompt": prompt,
            "reference": reference,
            "prediction": prediction,
            "is_correct": is_correct
        })
    
    # Calculate metrics
    accuracy = correct / total if total > 0 else 0
    
    metrics = {
        "accuracy": accuracy,
        "sample_count": total
    }
    
    # Save detailed results and metrics
    output_dir = os.path.dirname(holdout_file)
    results_file = os.path.join(output_dir, "holdout_results.json")
    with open(results_file, "w") as f:
        json.dump({
            "metrics": metrics,
            "results": results
        }, f, indent=2)
        
    return metrics

def train():
    """Main training function"""
    # Load configuration from CLI arguments and config file
    config = load_config_from_args()
    args = config.to_args()
    logger.info(f"Starting training with config: {config.to_dict()}")
    
    # Check if train file exists
    if not os.path.exists(args.train_file):
        logger.error(f"Training file not found: {args.train_file}")
        return 1
    
    # Determine validation file
    validation_file = args.train_file.replace(".json", "_validation.json")
    if not os.path.exists(validation_file):
        validation_file = None
    
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
    target_modules = args.target_modules.split(",") if isinstance(args.target_modules, str) else args.target_modules
    lora_config = LoraConfig(
        r=args.lora_r,
        lora_alpha=args.lora_alpha,
        target_modules=target_modules,
        lora_dropout=args.lora_dropout,
        bias="none",
        task_type=TaskType.CAUSAL_LM,
    )
    model = get_peft_model(model, lora_config)
    
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
    dataset, validation_data = load_training_data(args.train_file, validation_file)
    
    # Process the dataset
    def tokenize_function(example):
        return format_dataset(example, tokenizer, args.cutoff_len)
    
    tokenized_dataset = dataset.map(tokenize_function, batched=False)
    
    # Split into training and validation sets if no separate validation file
    if args.val_set_size > 0 and not validation_data:
        train_val = tokenized_dataset.train_test_split(
            test_size=args.val_set_size, shuffle=True, seed=42
        )
        train_data = train_val["train"]
        val_data = train_val["test"]
        logger.info(f"Split dataset: training on {len(train_data)} examples, validating on {len(val_data)} examples")
    elif validation_data:
        # Use provided validation data
        train_data = tokenized_dataset
        val_dataset = Dataset.from_dict(validation_data)
        val_data = val_dataset.map(tokenize_function, batched=False)
        logger.info(f"Using provided validation data: training on {len(train_data)} examples, validating on {len(val_data)} examples")
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
        metric_for_best_model="accuracy" if val_data else None,
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
        compute_metrics=compute_metrics if val_data else None,
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
    
    # Save version tag if provided
    version_tag = getattr(args, "version_tag", None) or f"v{datetime.now().strftime('%Y%m%d_%H%M%S')}"
    
    # Save training arguments and metrics
    train_metrics = trainer.state.log_history
    
    # Run validation on holdout set if available
    holdout_file = args.train_file.replace(".json", "_holdout.json")
    holdout_metrics = None
    if os.path.exists(holdout_file):
        logger.info(f"Running validation on holdout set: {holdout_file}")
        holdout_metrics = validate_model_on_holdout(model, tokenizer, holdout_file)
    
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
        "version_tag": version_tag,
        "training_metrics": train_metrics,
        "holdout_metrics": holdout_metrics
    }
    
    with open(os.path.join(args.output_dir, "training_info.json"), "w") as f:
        json.dump(training_args_dict, f, indent=2)
    
    logger.info("Training complete!")
    return 0

if __name__ == "__main__":
    exit_code = train()
    exit(exit_code)
