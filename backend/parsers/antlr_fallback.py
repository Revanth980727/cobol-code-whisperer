
import logging
import re
from typing import List, Dict, Any, Tuple
from models.cobol_chunk import CobolChunk
from parsers.chunk_extractor import ChunkExtractor

# Configure logging
logger = logging.getLogger("cobol_parser")

class AntlrFallbackExtractor:
    """Fallback extraction methods for when ANTLR parsing fails"""
    
    @staticmethod
    def extract_chunks(code: str) -> Tuple[List[Dict[str, Any]], List[CobolChunk]]:
        """
        Fallback method to extract chunks from code if ANTLR parsing fails
        
        Args:
            code: The COBOL code to parse
            
        Returns:
            Tuple of (divisions, chunks)
        """
        # Split the code into lines
        lines = code.split('\n')
        
        # Create empty divisions and chunks
        divisions = []
        chunks = []
        
        # Extract divisions using regex
        division_patterns = {
            "IDENTIFICATION DIVISION": r"^\s*IDENTIFICATION\s+DIVISION\s*\.",
            "ENVIRONMENT DIVISION": r"^\s*ENVIRONMENT\s+DIVISION\s*\.",
            "DATA DIVISION": r"^\s*DATA\s+DIVISION\s*\.",
            "PROCEDURE DIVISION": r"^\s*PROCEDURE\s+DIVISION\s*\."
        }
        
        division_locations = {}
        
        # Find the divisions
        for div_name, pattern in division_patterns.items():
            for i, line in enumerate(lines):
                if re.search(pattern, line, re.IGNORECASE):
                    division_locations[div_name] = i
                    divisions.append({
                        "division": div_name,
                        "line": i + 1,
                        "elements": []
                    })
                    break
        
        # Extract content for each division
        sorted_divs = sorted(division_locations.items(), key=lambda x: x[1])
        for i, (div_name, start_line) in enumerate(sorted_divs):
            end_line = len(lines) - 1
            if i < len(sorted_divs) - 1:
                end_line = sorted_divs[i+1][1] - 1
            
            division_content = '\n'.join(lines[start_line:end_line+1])
            chunks.append(CobolChunk(
                "division", 
                div_name, 
                division_content, 
                start_line + 1, 
                end_line + 1
            ))
        
        # Use the ChunkExtractor to extract sections and paragraphs
        divisions, chunks = ChunkExtractor.extract_sections_and_paragraphs(
            divisions, chunks, lines)
        
        return divisions, chunks
