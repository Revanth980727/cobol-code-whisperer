
from typing import Dict, Any

class CobolChunk:
    """Represents a meaningful chunk of COBOL code"""
    def __init__(self, chunk_type, name, content, start_line, end_line):
        self.chunk_type = chunk_type  # division, section, paragraph
        self.name = name
        self.content = content
        self.start_line = start_line
        self.end_line = end_line
    
    def to_dict(self):
        return {
            "type": self.chunk_type,
            "name": self.name,
            "content": self.content,
            "start_line": self.start_line,
            "end_line": self.end_line,
            "line_count": self.end_line - self.start_line + 1
        }
