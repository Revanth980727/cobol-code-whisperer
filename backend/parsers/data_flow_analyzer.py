
import re
import logging
from typing import List, Dict, Any, Optional

# Configure logging
logger = logging.getLogger("cobol_parser")

class DataFlowAnalyzer:
    """Analyzes data flow in COBOL code - tracking variables and their usage"""
    
    @staticmethod
    def analyze_data_flow(chunks):
        """Analyze data flow between variables in the code"""
        # Find all variables from DATA DIVISION
        variables = set()
        data_flow = {}
        
        for chunk in chunks:
            if chunk.chunk_type == "division" and "DATA DIVISION" in chunk.name:
                # Basic pattern to find variable declarations (simplified)
                var_pattern = r"^\s*\d+\s+([A-Za-z0-9-]+)"
                for match in re.finditer(var_pattern, chunk.content, re.MULTILINE):
                    variables.add(match.group(1).upper())
        
        # Track usage of variables in paragraphs
        for chunk in chunks:
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
                    data_flow[chunk.name] = variable_usage
        
        return data_flow
