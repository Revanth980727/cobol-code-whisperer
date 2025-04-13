
"""Service for LLM-based code analysis"""

import logging
from typing import Dict, Any, Optional
import os
from models.llm_response import LLMResponseParser
from services.model_providers import get_model_provider

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("llm_service")

class LLMService:
    """Service for LLM-based COBOL code analysis"""
    
    def __init__(self):
        # Get the appropriate model provider based on environment configuration
        self.model_provider = get_model_provider()
        
    def generate_text(self, prompt: str, max_tokens: int = 2000) -> str:
        """Generate text from the LLM based on a prompt."""
        if not self.model_provider.is_model_loaded():
            logger.warning("LLM not loaded, cannot generate text")
            return "LLM not loaded. Please check server logs."
            
        try:
            return self.model_provider.generate_text(prompt, max_tokens)
        except Exception as e:
            logger.error(f"Error generating text: {str(e)}")
            return f"Error generating text: {str(e)}"
            
    def analyze_code(self, code: str, chunk_type: str = None, chunk_name: str = None) -> Dict[str, Any]:
        """Analyze COBOL code using the LLM."""
        if not self.model_provider.is_model_loaded():
            logger.warning("LLM not loaded, cannot analyze code")
            return {
                "summary": "LLM not loaded. Please check server logs.",
                "business_rules": ["LLM analysis not available - model not loaded"]
            }
            
        try:
            # Select appropriate prompt template based on chunk information
            from prompt_templates import PromptTemplateEngine
            
            if chunk_type == "division" and "IDENTIFICATION DIVISION" in (chunk_name or ""):
                prompt = PromptTemplateEngine.data_division_prompt(code)
            elif chunk_type == "division" and "DATA DIVISION" in (chunk_name or ""):
                prompt = PromptTemplateEngine.data_division_prompt(code)
            elif chunk_type == "division" and "PROCEDURE DIVISION" in (chunk_name or ""):
                prompt = PromptTemplateEngine.procedure_division_prompt(code)
            elif chunk_type == "section":
                prompt = PromptTemplateEngine.section_prompt(chunk_name, code)
            elif chunk_type == "paragraph":
                prompt = PromptTemplateEngine.paragraph_prompt(chunk_name, code)
            else:
                prompt = PromptTemplateEngine.program_summary_prompt(code)
                
            # Generate text from the model
            response_text = self.generate_text(prompt)
            
            # Extract structured data from the response
            return LLMResponseParser.extract_json_from_response(response_text)
        except Exception as e:
            logger.error(f"Error analyzing code: {str(e)}")
            return {
                "summary": f"Error analyzing code: {str(e)}",
                "business_rules": ["Error occurred during analysis"]
            }
            
    def is_ready(self) -> bool:
        """Check if the LLM service is ready to use."""
        return self.model_provider.is_model_loaded()


# Singleton instance
llm_service = None

def get_llm_service() -> LLMService:
    """Get or initialize the LLM service."""
    global llm_service
    if llm_service is None:
        llm_service = LLMService()
    return llm_service
