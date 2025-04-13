
"""Base class for model providers"""

from abc import ABC, abstractmethod

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
