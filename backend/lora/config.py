
"""
Configuration for LoRA fine-tuning of language models.
Contains default hyperparameters and utilities for configuration management.
"""

import os
import json
import argparse
from typing import Dict, Any, Optional

DEFAULT_LORA_CONFIG = {
    # Model configuration
    "base_model": "meta-llama/Llama-3-8B-Instruct",  # Default base model
    "use_8bit": True,                # Use 8-bit quantization for efficiency
    
    # LoRA configuration
    "lora_r": 8,                     # LoRA attention dimension
    "lora_alpha": 16,                # Alpha parameter for LoRA scaling
    "lora_dropout": 0.05,            # Dropout probability for LoRA layers
    "target_modules": [              # Target modules for LoRA adaptation
        "q_proj",                    # Query projection
        "k_proj",                    # Key projection
        "v_proj",                    # Value projection
        "o_proj"                     # Output projection
    ],
    
    # Training configuration
    "batch_size": 64,                # Global batch size
    "micro_batch_size": 4,           # Per-device batch size
    "num_epochs": 3,                 # Number of training epochs
    "learning_rate": 3e-4,           # Learning rate
    "cutoff_len": 1024,              # Maximum token length
    "val_set_size": 0.1,             # Proportion of data to use for validation
    "warmup_steps": 100,             # Steps for learning rate warmup
    "logging_steps": 10,             # Frequency of logging
    "eval_steps": 50,                # Frequency of evaluation
    "save_steps": 200,               # Frequency of saving checkpoints
    
    # Output configuration
    "output_dir": "lora_models",     # Directory for saving models
    "logging_dir": "lora_logs"       # Directory for TensorBoard logs
}

class LoraConfig:
    """Class to manage LoRA fine-tuning configuration"""
    
    def __init__(self, config_path: Optional[str] = None):
        """Initialize configuration, optionally from a file"""
        self.config = DEFAULT_LORA_CONFIG.copy()
        
        if config_path and os.path.exists(config_path):
            self.load_config(config_path)
    
    def load_config(self, config_path: str) -> Dict[str, Any]:
        """Load configuration from a JSON file"""
        try:
            with open(config_path, 'r') as f:
                user_config = json.load(f)
                self.config.update(user_config)
            return self.config
        except Exception as e:
            print(f"Error loading config from {config_path}: {e}")
            return self.config
    
    def save_config(self, config_path: str) -> bool:
        """Save current configuration to a JSON file"""
        try:
            os.makedirs(os.path.dirname(config_path), exist_ok=True)
            with open(config_path, 'w') as f:
                json.dump(self.config, f, indent=2)
            return True
        except Exception as e:
            print(f"Error saving config to {config_path}: {e}")
            return False
    
    def update_config(self, new_config: Dict[str, Any]) -> Dict[str, Any]:
        """Update configuration with new values"""
        self.config.update(new_config)
        return self.config
    
    def to_dict(self) -> Dict[str, Any]:
        """Return configuration as a dictionary"""
        return self.config
    
    def to_args(self) -> argparse.Namespace:
        """Convert configuration to argparse Namespace for compatibility"""
        args = argparse.Namespace()
        for key, value in self.config.items():
            # Convert list to comma-separated string for target_modules
            if key == "target_modules" and isinstance(value, list):
                value = ",".join(value)
            setattr(args, key, value)
        return args

def parse_cli_args() -> Dict[str, Any]:
    """Parse command line arguments for LoRA training script"""
    parser = argparse.ArgumentParser(description="Fine-tune a model with LoRA")
    parser.add_argument("--config", type=str, help="Path to config JSON file")
    parser.add_argument("--model_name", type=str, help="Name of the base model to use")
    parser.add_argument("--train_file", type=str, required=True, help="Path to training data JSON file")
    parser.add_argument("--output_dir", type=str, help="Directory to save the model")
    parser.add_argument("--batch_size", type=int, help="Batch size")
    parser.add_argument("--micro_batch_size", type=int, help="Micro batch size")
    parser.add_argument("--num_epochs", type=int, help="Number of training epochs")
    parser.add_argument("--learning_rate", type=float, help="Learning rate")
    parser.add_argument("--cutoff_len", type=int, help="Max token length")
    parser.add_argument("--val_set_size", type=float, help="Proportion for validation set")
    parser.add_argument("--lora_r", type=int, help="LoRA attention dimension")
    parser.add_argument("--lora_alpha", type=int, help="Alpha parameter for LoRA scaling")
    parser.add_argument("--lora_dropout", type=float, help="Dropout for LoRA layers")
    parser.add_argument("--target_modules", type=str, help="Target modules for LoRA (comma-separated)")
    parser.add_argument("--use_8bit", action="store_true", help="Use 8bit precision")
    parser.add_argument("--eval_steps", type=int, help="Steps between evaluations")
    parser.add_argument("--save_steps", type=int, help="Steps between checkpoint saves")
    parser.add_argument("--logging_steps", type=int, help="Steps between logging")
    parser.add_argument("--warmup_steps", type=int, help="Warmup steps for learning rate scheduler")
    parser.add_argument("--logging_dir", type=str, help="Directory for tensorboard logs")
    parser.add_argument("--version_tag", type=str, help="Version tag for the model")
    
    # Parse known args only, to allow for script-specific arguments
    args, _ = parser.parse_known_args()
    
    # Convert to dictionary, excluding None values
    return {k: v for k, v in vars(args).items() if v is not None}

def load_config_from_args() -> LoraConfig:
    """Load configuration from command line arguments"""
    args_dict = parse_cli_args()
    config = LoraConfig()
    
    # If config file specified, load it first
    if "config" in args_dict:
        config.load_config(args_dict["config"])
        del args_dict["config"]  # Remove from args after loading
    
    # Update with any other specified CLI args
    config.update_config(args_dict)
    
    return config
