
import torch
import logging
import os
import json
import re
from typing import Dict, Any, Optional
from transformers import AutoModelForCausalLM, AutoTokenizer
from llama_cpp import Llama
from prompt_templates import PromptTemplateEngine

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("llm_service")

class LLMService:
    """Service for LLM-based COBOL code analysis"""
    
    def __init__(self):
        self.model_path = os.environ.get("LLAMA_MODEL_PATH", "meta-llama/Meta-Llama-3-8B-Instruct")
        self.use_llama_cpp = os.environ.get("USE_LLAMA_CPP", "false").lower() == "true"
        self.model_loaded = False
        self.llm = None
        self.model = None
        self.tokenizer = None
        
        # Initialize the model
        self._initialize_model()
        
    def _initialize_model(self):
        """Initialize the LLaMA model based on environment configuration."""
        try:
            if self.use_llama_cpp:
                # Use llama.cpp for optimized inference
                logger.info(f"Loading LLaMA 3 model using llama.cpp from {self.model_path}")
                self.llm = Llama(
                    model_path=self.model_path,
                    n_ctx=4096,  # Context window size
                    n_threads=4  # Adjust based on your CPU
                )
                self.model_loaded = self.llm is not None
                logger.info(f"LLaMA 3 model loaded successfully: {self.model_loaded}")
            else:
                # Use HuggingFace Transformers
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
            logger.error(f"Failed to load LLaMA model: {str(e)}")
            self.model_loaded = False
            
    def generate_text(self, prompt: str, max_tokens: int = 2000) -> str:
        """Generate text from the LLM based on a prompt."""
        if not self.model_loaded:
            logger.warning("LLM not loaded, cannot generate text")
            return "LLM not loaded. Please check server logs."
            
        try:
            if self.use_llama_cpp and self.llm:
                # Generate using llama.cpp
                response = self.llm(
                    prompt, 
                    max_tokens=max_tokens,
                    temperature=0.1,
                    top_p=0.9,
                    echo=False
                )
                return response["choices"][0]["text"]
            else:
                # Generate using HuggingFace Transformers
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
            logger.error(f"Error generating text: {str(e)}")
            return f"Error generating text: {str(e)}"
            
    def extract_json_from_response(self, response_text: str) -> Dict[str, Any]:
        """Extract JSON from LLM response."""
        try:
            # Try to extract JSON from the output
            json_match = re.search(r'```json\s*([\s\S]*?)\s*```', response_text)
            if json_match:
                return json.loads(json_match.group(1))
            else:
                # Try to extract content between any code blocks
                json_text = re.search(r'```([\s\S]*?)```', response_text)
                if json_text:
                    try:
                        return json.loads(json_text.group(1))
                    except:
                        # If that fails, try to manually extract summary and business rules
                        summary_match = re.search(r'"?summary"?\s*[:\=]\s*["\'](.*?)["\']', response_text, re.IGNORECASE | re.DOTALL)
                        rules_match = re.search(r'"?business_?rules"?\s*[:\=]\s*\[(.*?)\]', response_text, re.IGNORECASE | re.DOTALL)
                        
                        result = {
                            "summary": summary_match.group(1) if summary_match else "Summary extraction failed",
                            "business_rules": []
                        }
                        
                        if rules_match:
                            rules_text = rules_match.group(1)
                            rules = re.findall(r'"([^"]+)"', rules_text)
                            result["business_rules"] = rules if rules else ["Rules extraction failed"]
                        
                        return result
                else:
                    # Last resort fallback
                    return {
                        "summary": "Failed to extract structured summary from LLM output.",
                        "business_rules": ["Failed to extract business rules from LLM output."]
                    }
        except Exception as e:
            logger.error(f"Failed to parse LLM output as JSON: {e}")
            return {
                "summary": response_text[:500],  # First 500 chars as summary
                "business_rules": ["Failed to structure LLM output properly"]
            }
            
    def analyze_code(self, code: str, chunk_type: str = None, chunk_name: str = None) -> Dict[str, Any]:
        """Analyze COBOL code using the LLM."""
        if not self.model_loaded:
            logger.warning("LLM not loaded, cannot analyze code")
            return {
                "summary": "LLM not loaded. Please check server logs.",
                "business_rules": ["LLM analysis not available - model not loaded"]
            }
            
        try:
            # Select appropriate prompt template based on chunk information
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
            return self.extract_json_from_response(response_text)
        except Exception as e:
            logger.error(f"Error analyzing code: {str(e)}")
            return {
                "summary": f"Error analyzing code: {str(e)}",
                "business_rules": ["Error occurred during analysis"]
            }
            
    def is_ready(self) -> bool:
        """Check if the LLM service is ready to use."""
        return self.model_loaded


# Singleton instance
llm_service = None

def get_llm_service() -> LLMService:
    """Get or initialize the LLM service."""
    global llm_service
    if llm_service is None:
        llm_service = LLMService()
    return llm_service
