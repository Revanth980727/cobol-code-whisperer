
import re
import logging
from typing import Dict, Any, Optional
from parsers.dialect_detector import DialectDetector

# Configure logging
logger = logging.getLogger("cobol_parser")

class FallbackParser:
    """Simple fallback parser for when the main parser fails"""
    
    @staticmethod
    def parse(code):
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
        
        # Line-by-line fallback parsing
        lines = code.split("\n")
        line_analysis = []
        for i, line in enumerate(lines):
            line_type = "unknown"
            content = line.strip()
            
            if not content:
                line_type = "blank"
            elif content.startswith('*'):
                line_type = "comment"
            elif re.search(r'\bPERFORM\b', content, re.IGNORECASE):
                line_type = "perform"
            elif re.search(r'\bGO\s+TO\b', content, re.IGNORECASE):
                line_type = "goto"
            elif re.search(r'\bIF\b', content, re.IGNORECASE):
                line_type = "condition"
            elif re.search(r'\bMOVE\b', content, re.IGNORECASE):
                line_type = "move"
            elif re.search(r'\bDISPLAY\b', content, re.IGNORECASE):
                line_type = "display"
                
            line_analysis.append({"line_num": i+1, "content": content, "type": line_type})
        
        # Detect dialect
        dialect = DialectDetector.detect_dialect(code)
        
        return {
            "program_id": program_id,
            "divisions": divisions,
            "paragraphs": paragraphs,
            "line_analysis": line_analysis,
            "dialect": dialect,
            "warnings": warnings
        }
