
"""HuggingFace Transformers model provider implementation"""

import os
import logging
import torch
from typing import Optional
from .base_provider import BaseModelProvider

# Configure logging
logger = logging.getLogger("huggingface_provider")

class HuggingFaceProvider(BaseModelProvider):
    """Provider for HuggingFace Transformers models"""
    
    def __init__(self):
        self.model_path = os.environ.get("LLAMA_MODEL_PATH", "meta-llama/Meta-Llama-3-8B-Instruct")
        self.tokenizer = None
        self.model = None
        self.model_loaded = False
        
        # Initialize the model
        self._initialize_model()
        
    def _initialize_model(self):
        """Initialize the model using HuggingFace Transformers"""
        try:
            from transformers import AutoModelForCausalLM, AutoTokenizer
            
            logger.info(f"Loading LLaMA 3 model using HuggingFace Transformers from {self.model_path}")
            self.tokenizer = AutoTokenizer.from_pretrained(self.model_path)
            self.model = AutoModelForCausalLM.from_pretrained(
                self.model_path,
                torch_dtype=torch.float16,
                device_map="auto",
                low_cpu_mem_usage=True
            )
            self.model_loaded = (self.tokenizer is not None and self.model is not None)
            logger.info(f"LLaMA 3 model loaded successfully: {self.model_loaded}")
        except Exception as e:
            logger.error(f"Failed to load HuggingFace model: {str(e)}")
            self.model_loaded = False
        
    def is_model_loaded(self) -> bool:
        """Check if the model is loaded"""
        return self.model_loaded
        
    def generate_text(self, prompt: str, max_tokens: int = 2000) -> str:
        """Generate text using HuggingFace Transformers"""
        if not self.is_model_loaded():
            logger.warning("Model not loaded, cannot generate text")
            return "Model not loaded. Please check server logs."
            
        try:
            inputs = self.tokenizer(prompt, return_tensors="pt").to(self.model.device)
            with torch.no_grad():
                outputs = self.model.generate(
                    **inputs,
                    max_new_tokens=max_tokens,
                    temperature=0.1,
                    top_p=0.9,
                )
            return self.tokenizer.decode(outputs[0], skip_special_tokens=True)
        except Exception as e:
            logger.error(f"Error generating text with HuggingFace: {str(e)}")
            return f"Error generating text: {str(e)}"
            
    async def initialize_model(self) -> None:
        """Initialize the model asynchronously"""
        # Async initialization will be implemented in the future
        pass
        
    async def generate_text_async(self, prompt: str, max_tokens: int = 2000) -> str:
        """Generate text asynchronously"""
        # For now, we'll just call the synchronous method
        return self.generate_text(prompt, max_tokens)
        
    def get_model_info(self) -> dict:
        """Get information about the loaded model"""
        if not self.is_model_loaded():
            return {"status": "not_loaded"}
            
        return {
            "backend": "huggingface",
            "model_path": self.model_path,
            "device": str(next(self.model.parameters()).device),
            "model_type": "Transformers"
        }
