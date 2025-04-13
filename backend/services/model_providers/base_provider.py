
"""Base class for model providers"""

from abc import ABC, abstractmethod
from typing import Optional

class BaseModelProvider(ABC):
    """Abstract base class for model providers"""
    
    @abstractmethod
    def is_model_loaded(self) -> bool:
        """Check if the model is loaded"""
        pass
    
    @abstractmethod
    def generate_text(self, prompt: str, max_tokens: int = 2000) -> str:
        """Generate text from the model"""
        pass
    
    @abstractmethod
    async def initialize_model(self) -> None:
        """Initialize the model asynchronously"""
        pass
    
    @abstractmethod
    async def generate_text_async(self, prompt: str, max_tokens: int = 2000) -> str:
        """Generate text from the model asynchronously"""
        pass
    
    @abstractmethod
    def get_model_info(self) -> dict:
        """Get information about the loaded model"""
        pass
