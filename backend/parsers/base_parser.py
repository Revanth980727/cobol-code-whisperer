
import re
import logging
from typing import List, Dict, Any, Optional

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("cobol_parser")

class BaseCobolParser:
    """Base COBOL parser that handles basic parsing and initialization"""
    
    def __init__(self, code=None):
        self.code = code if code else ""
        self.lines = self.code.split('\n') if self.code else []
        self.chunks = []
        self.divisions = []
        self.call_graph = {}
        self.data_flow = {}
        
    def parse(self):
        """Parse the COBOL code structure"""
        self._extract_divisions()
        self._extract_sections_and_paragraphs()
        self._build_call_graph()
        self._analyze_data_flow()
        return {
            "divisions": self.divisions,
            "chunks": [chunk.to_dict() for chunk in self.chunks],
            "call_graph": self.call_graph,
            "data_flow": self.data_flow
        }
        
    def _extract_divisions(self):
        """Extract COBOL divisions from the code"""
        division_patterns = {
            "IDENTIFICATION DIVISION": r"^\s*IDENTIFICATION\s+DIVISION\s*\.",
            "ENVIRONMENT DIVISION": r"^\s*ENVIRONMENT\s+DIVISION\s*\.",
            "DATA DIVISION": r"^\s*DATA\s+DIVISION\s*\.",
            "PROCEDURE DIVISION": r"^\s*PROCEDURE\s+DIVISION\s*\."
        }
        
        division_locations = {}
        
        # Find the divisions
        for div_name, pattern in division_patterns.items():
            for i, line in enumerate(self.lines):
                if re.search(pattern, line, re.IGNORECASE):
                    division_locations[div_name] = i
                    self.divisions.append({
                        "division": div_name,
                        "line": i + 1,
                        "elements": []
                    })
        
        # Extract content for each division
        sorted_divs = sorted(division_locations.items(), key=lambda x: x[1])
        for i, (div_name, start_line) in enumerate(sorted_divs):
            end_line = len(self.lines) - 1
            if i < len(sorted_divs) - 1:
                end_line = sorted_divs[i+1][1] - 1
            
            from models.cobol_chunk import CobolChunk
            division_content = '\n'.join(self.lines[start_line:end_line+1])
            self.chunks.append(CobolChunk(
                "division", 
                div_name, 
                division_content, 
                start_line + 1, 
                end_line + 1
            ))
