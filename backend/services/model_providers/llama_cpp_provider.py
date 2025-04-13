
"""LLama CPP model provider implementation"""

import os
import logging
from typing import Optional
from .base_provider import BaseModelProvider

# Configure logging
logger = logging.getLogger("llama_cpp_provider")

class LlamaCppProvider(BaseModelProvider):
    """Provider for LLama.cpp models"""
    
    def __init__(self):
        self.model_path = os.environ.get("LLAMA_MODEL_PATH", "meta-llama/Meta-Llama-3-8B-Instruct")
        self.llm = None
        self.model_loaded = False
        
        # Initialize the model
        self._initialize_model()
        
    def _initialize_model(self):
        """Initialize the LLaMA model using llama.cpp"""
        try:
            from llama_cpp import Llama
            
            logger.info(f"Loading LLaMA 3 model using llama.cpp from {self.model_path}")
            self.llm = Llama(
                model_path=self.model_path,
                n_ctx=4096,  # Context window size
                n_threads=4  # Adjust based on your CPU
            )
            self.model_loaded = self.llm is not None
            logger.info(f"LLaMA 3 model loaded successfully: {self.model_loaded}")
        except Exception as e:
            logger.error(f"Failed to load LLaMA model: {str(e)}")
            self.model_loaded = False
        
    def is_model_loaded(self) -> bool:
        """Check if the model is loaded"""
        return self.model_loaded
        
    def generate_text(self, prompt: str, max_tokens: int = 2000) -> str:
        """Generate text using llama.cpp"""
        if not self.is_model_loaded():
            logger.warning("Model not loaded, cannot generate text")
            return "Model not loaded. Please check server logs."
            
        try:
            response = self.llm(
                prompt, 
                max_tokens=max_tokens,
                temperature=0.1,
                top_p=0.9,
                echo=False
            )
            return response["choices"][0]["text"]
        except Exception as e:
            logger.error(f"Error generating text with llama.cpp: {str(e)}")
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
            "backend": "llama_cpp",
            "model_path": self.model_path,
            "context_length": 4096,
            "model_type": "LLaMA"
        }
