
import re
import logging
import networkx as nx
from typing import List, Dict, Any, Optional

# Configure logging
logger = logging.getLogger("cobol_parser")

class ControlFlowAnalyzer:
    """Analyzes control flow in COBOL code"""
    
    @staticmethod
    def build_call_graph(chunks):
        """Build a call graph showing relationships between paragraphs"""
        # Map of paragraph/section names to their chunks
        name_to_chunk = {
            chunk.name: chunk for chunk in chunks 
            if chunk.chunk_type in ["paragraph", "section"]
        }
        
        # Find all PERFORM statements
        perform_pattern = r"\bPERFORM\s+([A-Za-z0-9-]+)"
        call_graph = {}
        
        for chunk in chunks:
            if chunk.chunk_type in ["paragraph", "section"]:
                calls = []
                for match in re.finditer(perform_pattern, chunk.content, re.IGNORECASE):
                    called_name = match.group(1)
                    if called_name in name_to_chunk:
                        calls.append(called_name)
                
                if calls:
                    call_graph[chunk.name] = calls
        
        return call_graph

    @staticmethod
    def extract_control_flow(code):
        """Extract control flow statements like GO TO and PERFORM"""
        flow_statements = []
        
        # Look for GO TO statements
        for match in re.finditer(r'\bGO\s+TO\s+([A-Za-z0-9-]+)', code, re.IGNORECASE):
            flow_statements.append({
                "type": "GO TO",
                "target": match.group(1),
                "position": match.start()
            })
        
        # Look for PERFORM statements
        for match in re.finditer(r'\bPERFORM\s+([A-Za-z0-9-]+)(?:\s+(?:THRU|THROUGH)\s+([A-Za-z0-9-]+))?', 
                                code, re.IGNORECASE):
            if match.group(2):
                flow_statements.append({
                    "type": "PERFORM THRU",
                    "start": match.group(1),
                    "end": match.group(2),
                    "position": match.start()
                })
            else:
                flow_statements.append({
                    "type": "PERFORM",
                    "target": match.group(1),
                    "position": match.start()
                })
        
        # Look for ALTER statements
        for match in re.finditer(r'\bALTER\s+([A-Za-z0-9-]+)\s+TO\s+(?:PROCEED\s+TO\s+)?([A-Za-z0-9-]+)', 
                                code, re.IGNORECASE):
            flow_statements.append({
                "type": "ALTER",
                "paragraph": match.group(1),
                "new_target": match.group(2),
                "position": match.start()
            })
            
        return flow_statements
    
    @staticmethod
    def analyze_control_flow(code):
        """Build a control flow graph from the code"""
        # Extract all paragraphs first
        paragraphs = {}
        para_pattern = r'^\s*([A-Za-z0-9-]+)\s*\.\s*$(.*?)(?=^\s*[A-Za-z0-9-]+\s*\.\s*$|\Z)'
        for match in re.finditer(para_pattern, code, re.MULTILINE | re.DOTALL):
            para_name = match.group(1).upper()
            para_content = match.group(2)
            paragraphs[para_name] = para_content
        
        # Build control flow graph
        flow_graph = {}
        for para_name, para_content in paragraphs.items():
            flow_graph[para_name] = {
                "calls": [],      # Paragraphs this paragraph calls via PERFORM
                "jumps_to": [],   # Paragraphs this paragraph jumps to via GO TO
                "thru_ranges": [] # Paragraph ranges this paragraph executes via PERFORM THRU
            }
            
            # Find PERFORM statements
            for match in re.finditer(r'\bPERFORM\s+([A-Za-z0-9-]+)(?:\s+(?:THRU|THROUGH)\s+([A-Za-z0-9-]+))?', 
                                   para_content, re.IGNORECASE):
                if match.group(2):  # PERFORM X THRU Y
                    flow_graph[para_name]["thru_ranges"].append({
                        "start": match.group(1),
                        "end": match.group(2)
                    })
                else:  # PERFORM X
                    flow_graph[para_name]["calls"].append(match.group(1))
            
            # Find GO TO statements
            for match in re.finditer(r'\bGO\s+TO\s+([A-Za-z0-9-]+)', para_content, re.IGNORECASE):
                flow_graph[para_name]["jumps_to"].append(match.group(1))
                
        return flow_graph
