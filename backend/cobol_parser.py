
import logging
import re
from typing import List, Dict, Any, Optional
from parsers.cobol_parser import CobolParser as BaseCobolParser
from analyzers.complexity_analyzer import calculate_complexity

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("cobol_parser")

class CobolParser(BaseCobolParser):
    """Enhanced COBOL parser with additional features for edge cases"""
    
    def __init__(self, code=None):
        super().__init__(code) if code else super().__init__()
        self.dialects = {
            "IBM": r"\bCOMP\b|\bGOBACK\b",
            "MF": r"\$SET\s+DIALECT|COMP-5|\bSTOP\s+RUN\b"
        }
    
    def parse_code(self, code, use_fallback=False):
        """Parse COBOL code with fallback mechanisms"""
        try:
            result = self.parse(code)
            return result
        except Exception as e:
            if use_fallback:
                logger.warning(f"Primary parser failed: {str(e)}. Using fallback parser.")
                return self.fallback_parse(code)
            else:
                logger.error(f"Parser error: {str(e)}")
                raise
    
    def fallback_parse(self, code):
        """Simple fallback parser for when the main parser fails"""
        # Basic structure extraction
        divisions = []
        program_id = None
        warnings = ["Used fallback parser due to syntax issues"]
        
        # Try to extract program ID
        program_id_match = re.search(r'PROGRAM-ID\.\s+([A-Za-z0-9-]+)', code, re.IGNORECASE)
        if program_id_match:
            program_id = program_id_match.group(1)
        
        # Try to identify divisions
        for div in ["IDENTIFICATION", "ENVIRONMENT", "DATA", "PROCEDURE"]:
            if re.search(rf'{div}\s+DIVISION', code, re.IGNORECASE):
                divisions.append(f"{div} DIVISION")
        
        # Basic paragraph extraction
        paragraphs = []
        para_pattern = r'^\s*([A-Za-z0-9-]+)\s*\.\s*$'
        for match in re.finditer(para_pattern, code, re.MULTILINE):
            para_name = match.group(1).upper()
            # Skip if this is likely not a paragraph (common keywords)
            if para_name not in ["PROGRAM-ID", "AUTHOR", "DATE-WRITTEN", "SECURITY"]:
                paragraphs.append({"name": para_name, "line": match.start()})
        
        # Detect dialect
        dialect = self.detect_dialect(code)
        
        return {
            "program_id": program_id,
            "divisions": divisions,
            "paragraphs": paragraphs,
            "dialect": dialect,
            "warnings": warnings
        }
    
    def detect_dialect(self, code):
        """Detect COBOL dialect based on syntax patterns"""
        dialect_scores = {}
        
        for dialect, pattern in self.dialects.items():
            matches = len(re.findall(pattern, code, re.IGNORECASE))
            dialect_scores[dialect] = matches
        
        if not any(dialect_scores.values()):
            return "UNKNOWN"
        
        return max(dialect_scores, key=dialect_scores.get)
    
    def contains_verb(self, code, verb):
        """Check if code contains a specific COBOL verb"""
        pattern = rf'\b{verb}\b'
        return bool(re.search(pattern, code, re.IGNORECASE))
    
    def extract_control_flow(self, code):
        """Extract control flow statements like GO TO and PERFORM"""
        flow_statements = []
        
        # Look for GO TO statements
        for match in re.finditer(r'\bGO\s+TO\s+([A-Za-z0-9-]+)', code, re.IGNORECASE):
            flow_statements.append(f"GO TO {match.group(1)}")
        
        # Look for PERFORM statements
        for match in re.finditer(r'\bPERFORM\s+([A-Za-z0-9-]+)(?:\s+THRU\s+([A-Za-z0-9-]+))?', 
                                code, re.IGNORECASE):
            if match.group(2):
                flow_statements.append(f"PERFORM {match.group(1)} THRU {match.group(2)}")
            else:
                flow_statements.append(f"PERFORM {match.group(1)}")
        
        return flow_statements

def parse_cobol_structure(code: str) -> List[Dict[str, Any]]:
    """Parse COBOL code structure using enhanced parser"""
    parser = CobolParser(code)
    parsed_data = parser.parse()
    return parsed_data["divisions"]

def chunk_cobol_code(code: str) -> List[Dict[str, Any]]:
    """Chunk COBOL code into meaningful segments using enhanced parser"""
    parser = CobolParser(code)
    parsed_data = parser.parse()
    return parsed_data["chunks"]
