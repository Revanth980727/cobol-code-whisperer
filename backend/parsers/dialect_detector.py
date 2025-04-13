
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
            "GNU": r"\bCOBOL\s+85\b|\bANSI\b",
            "ANSI85": r"\bREPLACE\b.*\bBY\b|\bINITIALIZE\b|\bREFERENCE\b|\bCONTENT\b",
            "VS-COBOL-II": r"\bEXEC\s+CICS\b|\bCICS\b|\bADDRESS\s+OF\b|\bEIBCALEN\b"
        }
    
    @staticmethod
    def detect_dialect(code):
        """Detect COBOL dialect based on syntax patterns"""
        dialect_scores = {}
        dialect_patterns = DialectDetector.get_dialect_patterns()
        
        for dialect, pattern in dialect_patterns.items():
            matches = len(re.findall(pattern, code, re.IGNORECASE))
            dialect_scores[dialect] = matches
        
        # Check for special indicators in compiler directives
        if re.search(r"\$SET\s+DIALECT\(MF\)", code, re.IGNORECASE):
            dialect_scores["MF"] += 5
        if re.search(r"\$SET\s+DIALECT\(IBM\)", code, re.IGNORECASE):
            dialect_scores["IBM"] += 5
        
        # Check for specific IBM Enterprise COBOL features
        if re.search(r"\bJSON\s+PARSE\b|\bJSON\s+GENERATE\b|\bXML\s+PARSE\b", code, re.IGNORECASE):
            dialect_scores["IBM"] += 3
        
        # Check for specific MicroFocus features
        if re.search(r"\$DISPLAY|\$SET|\$REGION|\$DIR", code):
            dialect_scores["MF"] += 3
        
        # Check for specific ACUCOBOL features
        if re.search(r"\bACUCOBOL\b|\bACU\b|\bACCEPT\s+.*\s+FROM\s+SCREEN\b", code, re.IGNORECASE):
            dialect_scores["ACUCOBOL"] += 3
        
        if not any(dialect_scores.values()):
            return "UNKNOWN"
        
        return max(dialect_scores, key=dialect_scores.get)
    
    @staticmethod
    def analyze_dialect_features(code):
        """Analyze dialect-specific features in the code"""
        features = {
            "has_inline_comments": bool(re.search(r"\*>.*$", code, re.MULTILINE)),
            "has_old_style_comments": bool(re.search(r"^\s*\*.*$", code, re.MULTILINE)),
            "has_cics": bool(re.search(r"\bEXEC\s+CICS\b", code, re.IGNORECASE)),
            "has_sql": bool(re.search(r"\bEXEC\s+SQL\b", code, re.IGNORECASE)),
            "has_json": bool(re.search(r"\bJSON\s+", code, re.IGNORECASE)),
            "has_xml": bool(re.search(r"\bXML\s+", code, re.IGNORECASE))
        }
        
        # Count occurrences of key verbs
        verb_count = {}
        key_verbs = ["PERFORM", "MOVE", "IF", "EVALUATE", "GO TO", "CALL", "DISPLAY"]
        for verb in key_verbs:
            verb_count[verb] = len(re.findall(rf"\b{verb}\b", code, re.IGNORECASE))
        
        features["verb_count"] = verb_count
        
        return features
