
import re
import logging
from typing import List, Dict, Any
import networkx as nx
from models.cobol_chunk import CobolChunk
from analyzers.complexity_analyzer import calculate_complexity

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("cobol_parser")

class CobolParser:
    """Enhanced COBOL parser that extracts structure, chunks, and relationships"""
    
    def __init__(self, code):
        self.code = code
        self.lines = code.split('\n')
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
            
            division_content = '\n'.join(self.lines[start_line:end_line+1])
            self.chunks.append(CobolChunk(
                "division", 
                div_name, 
                division_content, 
                start_line + 1, 
                end_line + 1
            ))
    
    def _extract_sections_and_paragraphs(self):
        """Extract sections and paragraphs from the PROCEDURE DIVISION"""
        procedure_div_index = -1
        for i, chunk in enumerate(self.chunks):
            if chunk.chunk_type == "division" and "PROCEDURE DIVISION" in chunk.name:
                procedure_div_index = i
                break
        
        if procedure_div_index < 0:
            return
        
        procedure_div = self.chunks[procedure_div_index]
        procedure_lines = procedure_div.content.split('\n')
        
        # Find sections
        section_pattern = r"^\s*([A-Za-z0-9-]+)\s+SECTION\s*\."
        section_locations = {}
        
        for i, line in enumerate(procedure_lines):
            section_match = re.search(section_pattern, line, re.IGNORECASE)
            if section_match:
                section_name = section_match.group(1)
                section_line = procedure_div.start_line + i
                section_locations[section_name] = section_line
                
                # Add section to the appropriate division
                for div in self.divisions:
                    if "PROCEDURE DIVISION" in div["division"]:
                        div["elements"].append({
                            "name": section_name,
                            "description": f"{section_name} section"
                        })
        
        # Extract content for each section
        sorted_sections = sorted(section_locations.items(), key=lambda x: x[1])
        for i, (section_name, section_line) in enumerate(sorted_sections):
            abs_start_line = section_line
            rel_start_line = section_line - procedure_div.start_line
            
            end_rel_line = len(procedure_lines) - 1
            if i < len(sorted_sections) - 1:
                end_rel_line = sorted_sections[i+1][1] - procedure_div.start_line - 1
            
            section_content = '\n'.join(procedure_lines[rel_start_line:end_rel_line+1])
            abs_end_line = procedure_div.start_line + end_rel_line
            
            self.chunks.append(CobolChunk(
                "section", 
                section_name, 
                section_content, 
                abs_start_line, 
                abs_end_line
            ))
            
            # Now find paragraphs within this section
            self._extract_paragraphs(section_content, section_name, abs_start_line, abs_end_line)
        
        # If no sections found, extract paragraphs directly from procedure division
        if not section_locations:
            self._extract_paragraphs(procedure_div.content, None, procedure_div.start_line, procedure_div.end_line)
    
    def _extract_paragraphs(self, content, parent_section, start_line, end_line):
        """Extract paragraphs from a section or procedure division"""
        lines = content.split('\n')
        paragraph_pattern = r"^\s*([A-Za-z0-9-]+)\s*\."
        para_locations = {}
        
        for i, line in enumerate(lines):
            # Skip lines that are section declarations
            if re.search(r"\bSECTION\b", line, re.IGNORECASE):
                continue
                
            para_match = re.search(paragraph_pattern, line, re.IGNORECASE)
            if para_match:
                para_name = para_match.group(1)
                # Skip if this is likely not a paragraph (common keywords)
                if para_name.upper() in ["PROGRAM-ID", "AUTHOR", "DATE-WRITTEN", "SECURITY"]:
                    continue
                    
                para_line = start_line + i
                para_locations[para_name] = para_line
                
                # Add paragraph to the appropriate division
                for div in self.divisions:
                    if "PROCEDURE DIVISION" in div["division"]:
                        div["elements"].append({
                            "name": para_name,
                            "description": f"{para_name} paragraph" + (f" in {parent_section} section" if parent_section else "")
                        })
        
        # Extract content for each paragraph
        sorted_paras = sorted(para_locations.items(), key=lambda x: x[1])
        for i, (para_name, para_line) in enumerate(sorted_paras):
            rel_start_line = para_line - start_line
            
            end_rel_line = len(lines) - 1
            if i < len(sorted_paras) - 1:
                end_rel_line = sorted_paras[i+1][1] - start_line - 1
            
            para_content = '\n'.join(lines[rel_start_line:end_rel_line+1])
            abs_end_line = start_line + end_rel_line
            
            self.chunks.append(CobolChunk(
                "paragraph", 
                para_name, 
                para_content, 
                para_line, 
                abs_end_line
            ))
    
    def _build_call_graph(self):
        """Build a call graph showing relationships between paragraphs"""
        # Map of paragraph/section names to their chunks
        name_to_chunk = {
            chunk.name: chunk for chunk in self.chunks 
            if chunk.chunk_type in ["paragraph", "section"]
        }
        
        # Find all PERFORM statements
        perform_pattern = r"\bPERFORM\s+([A-Za-z0-9-]+)"
        
        for chunk in self.chunks:
            if chunk.chunk_type in ["paragraph", "section"]:
                calls = []
                for match in re.finditer(perform_pattern, chunk.content, re.IGNORECASE):
                    called_name = match.group(1)
                    if called_name in name_to_chunk:
                        calls.append(called_name)
                
                if calls:
                    self.call_graph[chunk.name] = calls
    
    def _analyze_data_flow(self):
        """Analyze data flow between variables in the code"""
        # Find all variables from DATA DIVISION
        variables = set()
        
        for chunk in self.chunks:
            if chunk.chunk_type == "division" and "DATA DIVISION" in chunk.name:
                # Basic pattern to find variable declarations (simplified)
                var_pattern = r"^\s*\d+\s+([A-Za-z0-9-]+)"
                for match in re.finditer(var_pattern, chunk.content, re.MULTILINE):
                    variables.add(match.group(1).upper())
        
        # Track usage of variables in paragraphs
        for chunk in self.chunks:
            if chunk.chunk_type == "paragraph":
                variable_usage = []
                for var in variables:
                    # Check for MOVE statements (writing to variable)
                    move_pattern = rf"\bMOVE\s+[A-Za-z0-9-]+\s+TO\s+{var}\b"
                    if re.search(move_pattern, chunk.content, re.IGNORECASE):
                        variable_usage.append({"variable": var, "operation": "write"})
                    
                    # Check for variable reads (in conditions or calculations)
                    read_pattern = rf"\b{var}\b"
                    if re.search(read_pattern, chunk.content, re.IGNORECASE):
                        # Exclude the MOVE TO cases we already captured
                        move_to_pattern = rf"\bMOVE\s+[A-Za-z0-9-]+\s+TO\s+{var}\b"
                        if not re.search(move_to_pattern, chunk.content, re.IGNORECASE):
                            variable_usage.append({"variable": var, "operation": "read"})
                
                if variable_usage:
                    self.data_flow[chunk.name] = variable_usage
