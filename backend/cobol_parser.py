
import logging
import re
from typing import List, Dict, Any, Optional
from parsers.cobol_parser import CobolParser as BaseCobolParser
from analyzers.complexity_analyzer import calculate_complexity
from parsers.dialect_detector import DialectDetector
from parsers.preprocessor import CobolPreprocessor
from parsers.flow_analyzer import ControlFlowAnalyzer
from parsers.fallback_parser import FallbackParser

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("cobol_parser")

class CobolParser(BaseCobolParser):
    """Enhanced COBOL parser with additional features for edge cases"""
    
    def __init__(self, code=None):
        super().__init__(code) if code else super().__init__()
        self.dialects = DialectDetector.get_dialect_patterns()
        
        # For tracking copy statements
        self.copy_modules = {}
        # For tracking flow control
        self.control_flow_graph = {}
    
    # The main parser functionality is now inherited from BaseCobolParser
    # We only need to add or override specific methods for edge cases
    
    def parse_code(self, code, use_fallback=False):
        """Parse COBOL code with fallback mechanisms"""
        try:
            # Preprocess COPY and REPLACE statements first
            preprocessed_code = self.preprocess_code(code)
            
            # Detect dialect for adaptive parsing
            dialect = self.detect_dialect(preprocessed_code)
            logger.info(f"Detected COBOL dialect: {dialect}")
            
            # Parse the preprocessed code
            result = self.parse()
            result["dialect"] = dialect
            
            # Build control flow information
            result["control_flow"] = self.analyze_control_flow(preprocessed_code)
            
            return result
        except Exception as e:
            if use_fallback:
                logger.warning(f"Primary parser failed: {str(e)}. Using fallback parser.")
                return self.fallback_parse(code)
            else:
                logger.error(f"Parser error: {str(e)}")
                raise
    
    def contains_verb(self, code, verb):
        """Check if code contains a specific COBOL verb"""
        pattern = rf'\b{verb}\b'
        return bool(re.search(pattern, code, re.IGNORECASE))
