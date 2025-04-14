
import logging
import os
from typing import Dict, Any, Optional
from parsers.cobol_parser import CobolParser
from analyzers.complexity_analyzer import calculate_complexity
from services.file_storage import store_file, get_file
from services.llm_service import get_llm_service

# Configure logging
logger = logging.getLogger("cobol-service")

# Check if we should use ANTLR parser from environment variable
USE_ANTLR = os.environ.get("USE_ANTLR", "true").lower() in ("true", "1", "yes", "y")

def analyze_cobol_code(file_id: str, filename: str, file_content: bytes) -> Dict[str, Any]:
    """Analyze COBOL code and return the analysis results."""
    # Decode the content
    try:
        code = file_content.decode('utf-8')
    except UnicodeDecodeError:
        # Try another encoding if utf-8 fails
        try:
            code = file_content.decode('latin1')
        except:
            logger.error("Could not decode file content")
            return {"error": "Could not decode file content"}
    
    # Store the file content
    store_file(file_id, filename, file_content, len(code.splitlines()))
    
    # Parse code into chunks for analysis
    parser = CobolParser(code)
    
    # Log whether ANTLR is available
    logger.info(f"ANTLR parser available: {parser.antlr_parser_available}")
    logger.info(f"Using ANTLR parser: {USE_ANTLR}")
    
    # Parse with appropriate parser
    parsed_data = parser.parse_code(code, use_fallback=True, use_antlr=USE_ANTLR)
    
    # Calculate complexity metrics
    complexity = calculate_complexity(code)
    
    # Analyze with LLM
    llm_analysis = analyze_with_llm(code)
    
    # Combine all analysis results
    analysis_result = {
        "file_id": file_id,
        "summary": llm_analysis.get("summary", "Analysis not available"),
        "business_rules": llm_analysis.get("business_rules", []),
        "code_structure": parsed_data["divisions"],
        "complexity": complexity,
        "call_graph": parsed_data["call_graph"],
        "data_flow": parsed_data["data_flow"],
        "chunks": [chunk for chunk in parsed_data["chunks"] if chunk["type"] != "division"][:10],  # Return non-division chunks, limited to 10
        "parser_used": "ANTLR" if (USE_ANTLR and parser.antlr_parser_available) else "Standard"
    }
    
    return analysis_result

def analyze_with_llm(code: str, structured_parsing: bool = True) -> dict:
    """Use LLM to analyze COBOL code with chunking."""
    llm_service = get_llm_service()
    
    if not llm_service.is_ready():
        # Fallback if model is not loaded
        logger.warning("LLM not available. Using fallback analysis.")
        return {
            "summary": "LLM not available. Using fallback analysis.",
            "business_rules": [
                "Automatic analysis not available - model not loaded",
                "Please check server logs for model loading errors"
            ]
        }
    
    try:
        # Generate high-level summary
        high_level_analysis = llm_service.analyze_code(code)
        
        # If we're not doing structured parsing, return the high-level analysis
        if not structured_parsing:
            return high_level_analysis
            
        # Otherwise, proceed with detailed chunk analysis
        # Parse the code into chunks for detailed analysis
        parser = CobolParser(code)
        parsed_data = parser.parse_code(code, use_antlr=USE_ANTLR)
        chunks = parsed_data["chunks"]
        
        # Select important chunks to analyze (to avoid too many API calls)
        # For now, let's analyze all divisions and a few key paragraphs/sections
        chunks_to_analyze = [chunk for chunk in chunks if chunk["type"] == "division"]
        
        # Add some important paragraphs/sections if they exist
        procedure_chunks = [
            chunk for chunk in chunks 
            if chunk["type"] in ["section", "paragraph"] and 
            chunk["line_count"] > 5  # Only substantial paragraphs
        ]
        
        # Sort by length and take top 3
        procedure_chunks.sort(key=lambda x: x["line_count"], reverse=True)
        chunks_to_analyze.extend(procedure_chunks[:3])
        
        # Analyze each selected chunk
        chunk_analyses = []
        for chunk in chunks_to_analyze:
            analysis = llm_service.analyze_code(
                chunk["content"], 
                chunk_type=chunk["type"], 
                chunk_name=chunk["name"]
            )
            
            chunk_analyses.append({
                "chunk_type": chunk["type"],
                "chunk_name": chunk["name"],
                "analysis": analysis
            })
        
        # Extract additional business rules from chunk analyses
        additional_rules = []
        for chunk_analysis in chunk_analyses:
            analysis = chunk_analysis["analysis"]
            
            # Look for business rules in the analysis
            if "business_rules" in analysis and isinstance(analysis["business_rules"], list):
                for rule in analysis["business_rules"]:
                    if rule not in additional_rules and rule not in high_level_analysis.get("business_rules", []):
                        additional_rules.append(rule)
        
        # Add additional business rules to the high-level analysis
        existing_rules = high_level_analysis.get("business_rules", [])
        existing_rules.extend(additional_rules)
        
        high_level_analysis["business_rules"] = existing_rules
        
        return high_level_analysis
    except Exception as e:
        logger.error(f"LLM analysis failed: {e}")
        return {
            "summary": f"LLM analysis failed: {str(e)}",
            "business_rules": ["Error occurred during analysis"]
        }

def get_file_content(file_id: str) -> Optional[Dict[str, Any]]:
    """Get file content by ID."""
    file_data = get_file(file_id)
    
    if not file_data:
        return None
    
    return {
        "filename": file_data["filename"],
        "content": file_data["content"].decode('utf-8'),
        "lines": file_data["lines"]
    }
