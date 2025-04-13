
"""Models for LLM responses and related data structures"""

from typing import Dict, Any, List, Optional
import re
import json
import logging

# Configure logging
logger = logging.getLogger("llm_response")

class LLMResponseParser:
    """Parser for extracting structured data from LLM responses"""
    
    @staticmethod
    def extract_json_from_response(response_text: str) -> Dict[str, Any]:
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
