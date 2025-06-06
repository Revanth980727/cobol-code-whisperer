
import logging
import re
from typing import List, Dict, Any, Optional
from parsers.cobol_parser import CobolParser as BaseCobolParser
from analyzers.complexity_analyzer import calculate_complexity
from parsers.dialect_detector import DialectDetector
from parsers.preprocessor import CobolPreprocessor
from parsers.flow_analyzer import ControlFlowAnalyzer
from parsers.fallback_parser import FallbackParser
from parsers.antlr_parser import AntlrCobolParser

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("cobol_parser")

class CobolParser(BaseCobolParser):
    """Enhanced COBOL parser with additional features for edge cases"""
    
    def __init__(self, code=None):
        super().__init__(code) if code else super().__init__()
        self.dialects = DialectDetector.get_dialect_patterns()
        self.preprocessor = CobolPreprocessor()
        
        # For tracking copy statements
        self.copy_modules = {}
        # For tracking flow control
        self.control_flow_graph = {}
        # Check if ANTLR parser is available
        self.antlr_parser_available = AntlrCobolParser.is_available()
        logger.info(f"ANTLR parser available: {self.antlr_parser_available}")
    
    def parse_code(self, code, use_fallback=False, use_antlr=True):
        """Parse COBOL code with ANTLR and fallback mechanisms"""
        try:
            # Preprocess COPY and REPLACE statements first
            preprocessed_code = self.preprocessor.preprocess_code(code)
            
            # Detect dialect for adaptive parsing
            dialect = self.detect_dialect(preprocessed_code)
            logger.info(f"Detected COBOL dialect: {dialect}")
            
            # Try ANTLR parser first if available and enabled
            if use_antlr and self.antlr_parser_available:
                logger.info("Attempting to parse with ANTLR parser")
                try:
                    antlr_parser = AntlrCobolParser()
                    result = antlr_parser.parse(preprocessed_code)
                    if result:
                        logger.info("Successfully parsed with ANTLR parser")
                        result["dialect"] = dialect
                        result["control_flow"] = self.analyze_control_flow(preprocessed_code)
                        return result
                    else:
                        logger.warning("ANTLR parser returned None result. Falling back to standard parser.")
                except Exception as e:
                    logger.warning(f"ANTLR parser failed: {str(e)}. Falling back to standard parser.")
            
            # Fallback to the standard parser
            logger.info("Using standard COBOL parser")
            # Store the code for analysis
            self.code = preprocessed_code
            self.lines = preprocessed_code.split('\n')
            
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
    
    def detect_dialect(self, code):
        """Detect COBOL dialect based on syntax patterns"""
        return DialectDetector.detect_dialect(code)
    
    def preprocess_code(self, code):
        """Preprocess COBOL code to handle COPY and REPLACE statements"""
        return self.preprocessor.preprocess_code(code)
    
    def fallback_parse(self, code):
        """Simple fallback parser for when the main parser fails"""
        return FallbackParser.parse(code)
    
    def analyze_control_flow(self, code):
        """Build a control flow graph from the code"""
        return ControlFlowAnalyzer.analyze_control_flow(code)
    
    def contains_verb(self, code, verb):
        """Check if code contains a specific COBOL verb"""
        pattern = rf'\b{verb}\b'
        return bool(re.search(pattern, code, re.IGNORECASE))
