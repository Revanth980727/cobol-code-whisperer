
"""Model provider initialization and factory module"""

import os
from typing import Union
import logging

logger = logging.getLogger("model_providers")

def get_model_provider():
    """Factory function to get the appropriate model provider based on environment configuration"""
    use_llama_cpp = os.environ.get("USE_LLAMA_CPP", "false").lower() == "true"
    
    if use_llama_cpp:
        from .llama_cpp_provider import LlamaCppProvider
        return LlamaCppProvider()
    else:
        from .huggingface_provider import HuggingFaceProvider
        return HuggingFaceProvider()
