
import re
import logging
from typing import Dict, Any, Optional

# Configure logging
logger = logging.getLogger("cobol_parser")

class DialectDetector:
    """Detects COBOL dialect based on syntax patterns"""
    
    @staticmethod
    def get_dialect_patterns():
        """Get patterns that identify different COBOL dialects"""
        return {
            "IBM": r"\bCOMP\b|\bGOBACK\b|\bGO\s+TO\b|\bALTER\b|\bPERFORM\s+.*\s+THRU\b",
            "MF": r"\$SET\s+DIALECT|COMP-5|\bSTOP\s+RUN\b|\bPERFORM\s+.*\s+THROUGH\b",
            "ACUCOBOL": r"\$SET\s+OKFORAC|\bACU\b|\bSCREEN\s+SECTION\b",
            "GNU": r"\bCOBOL\s+85\b|\bANSI\b"
        }
    
    @staticmethod
    def detect_dialect(code):
        """Detect COBOL dialect based on syntax patterns"""
        dialect_scores = {}
        dialect_patterns = DialectDetector.get_dialect_patterns()
        
        for dialect, pattern in dialect_patterns.items():
            matches = len(re.findall(pattern, code, re.IGNORECASE))
            dialect_scores[dialect] = matches
        
        if not any(dialect_scores.values()):
            return "UNKNOWN"
        
        return max(dialect_scores, key=dialect_scores.get)
