import logging
from typing import List, Dict, Any
from parsers.cobol_parser import CobolParser
from analyzers.complexity_analyzer import calculate_complexity

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("cobol_parser")

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
