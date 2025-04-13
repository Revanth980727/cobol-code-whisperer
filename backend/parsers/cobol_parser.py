
import logging
from typing import List, Dict, Any, Optional
from models.cobol_chunk import CobolChunk
from parsers.base_parser import BaseCobolParser
from parsers.chunk_extractor import ChunkExtractor
from parsers.flow_analyzer import ControlFlowAnalyzer
from parsers.data_flow_analyzer import DataFlowAnalyzer
from parsers.dialect_detector import DialectDetector
from parsers.preprocessor import CobolPreprocessor
from parsers.fallback_parser import FallbackParser

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("cobol_parser")

class CobolParser(BaseCobolParser):
    """Enhanced COBOL parser that extracts structure, chunks, and relationships"""
    
    def __init__(self, code=None):
        super().__init__(code)
        self.preprocessor = CobolPreprocessor()
        
    def parse(self):
        """Parse the COBOL code structure"""
        self._extract_divisions()
        self.divisions, self.chunks = ChunkExtractor.extract_sections_and_paragraphs(
            self.divisions, self.chunks, self.lines)
        self.call_graph = ControlFlowAnalyzer.build_call_graph(self.chunks)
        self.data_flow = DataFlowAnalyzer.analyze_data_flow(self.chunks)
        return {
            "divisions": self.divisions,
            "chunks": [chunk.to_dict() for chunk in self.chunks],
            "call_graph": self.call_graph,
            "data_flow": self.data_flow
        }
    
    def contains_verb(self, code, verb):
        """Check if code contains a specific COBOL verb"""
        pattern = rf'\b{verb}\b'
        import re
        return bool(re.search(pattern, code, re.IGNORECASE))
    
    def extract_control_flow(self, code):
        """Extract control flow statements like GO TO and PERFORM"""
        return ControlFlowAnalyzer.extract_control_flow(code)
    
    def analyze_control_flow(self, code):
        """Build a control flow graph from the code"""
        return ControlFlowAnalyzer.analyze_control_flow(code)
    
    def detect_dialect(self, code):
        """Detect COBOL dialect based on syntax patterns"""
        return DialectDetector.detect_dialect(code)
    
    def preprocess_code(self, code):
        """Preprocess COBOL code to handle COPY and REPLACE statements"""
        return self.preprocessor.preprocess_code(code)
    
    def fallback_parse(self, code):
        """Simple fallback parser for when the main parser fails"""
        return FallbackParser.parse(code)
    
    def parse_code(self, code, use_fallback=False):
        """Parse COBOL code with fallback mechanisms"""
        try:
            # Store the code for analysis
            self.code = code
            self.lines = code.split('\n')
            
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
