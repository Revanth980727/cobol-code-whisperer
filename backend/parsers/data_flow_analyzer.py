
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
        
        # Also look for WORKING-STORAGE SECTION variables
        for chunk in chunks:
            if "WORKING-STORAGE SECTION" in chunk.content:
                # Pattern for PIC clause variables
                pic_pattern = r"^\s*\d+\s+([A-Za-z0-9-]+).*PIC"
                for match in re.finditer(pic_pattern, chunk.content, re.MULTILINE | re.IGNORECASE):
                    variables.add(match.group(1).upper())
                    
                # Pattern for REDEFINES clause variables
                redefines_pattern = r"^\s*\d+\s+([A-Za-z0-9-]+).*REDEFINES"
                for match in re.finditer(redefines_pattern, chunk.content, re.MULTILINE | re.IGNORECASE):
                    variables.add(match.group(1).upper())
                    
                # Pattern for VALUE clause variables
                value_pattern = r"^\s*\d+\s+([A-Za-z0-9-]+).*VALUE"
                for match in re.finditer(value_pattern, chunk.content, re.MULTILINE | re.IGNORECASE):
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
                    
                    # Check for COMPUTE statements (writing to variable)
                    compute_pattern = rf"\bCOMPUTE\s+{var}\s*="
                    if re.search(compute_pattern, chunk.content, re.IGNORECASE):
                        variable_usage.append({"variable": var, "operation": "write"})
                    
                    # Check for ADD TO statements (writing to variable)
                    add_pattern = rf"\bADD\s+[A-Za-z0-9-]+\s+TO\s+{var}\b"
                    if re.search(add_pattern, chunk.content, re.IGNORECASE):
                        variable_usage.append({"variable": var, "operation": "write"})
                    
                    # Check for SUBTRACT FROM statements (writing to variable)
                    subtract_pattern = rf"\bSUBTRACT\s+[A-Za-z0-9-]+\s+FROM\s+{var}\b"
                    if re.search(subtract_pattern, chunk.content, re.IGNORECASE):
                        variable_usage.append({"variable": var, "operation": "write"})
                    
                    # Check for MULTIPLY BY statements (writing to variable)
                    multiply_pattern = rf"\bMULTIPLY\s+[A-Za-z0-9-]+\s+BY\s+{var}\b"
                    if re.search(multiply_pattern, chunk.content, re.IGNORECASE):
                        variable_usage.append({"variable": var, "operation": "write"})
                    
                    # Check for DIVIDE INTO statements (writing to variable)
                    divide_pattern = rf"\bDIVIDE\s+[A-Za-z0-9-]+\s+INTO\s+{var}\b"
                    if re.search(divide_pattern, chunk.content, re.IGNORECASE):
                        variable_usage.append({"variable": var, "operation": "write"})
                    
                    # Check for variable reads (in conditions or calculations)
                    read_pattern = rf"\b{var}\b"
                    if re.search(read_pattern, chunk.content, re.IGNORECASE):
                        # Exclude the cases we already captured
                        skip_patterns = [
                            rf"\bMOVE\s+[A-Za-z0-9-]+\s+TO\s+{var}\b",
                            rf"\bCOMPUTE\s+{var}\s*=",
                            rf"\bADD\s+[A-Za-z0-9-]+\s+TO\s+{var}\b",
                            rf"\bSUBTRACT\s+[A-Za-z0-9-]+\s+FROM\s+{var}\b",
                            rf"\bMULTIPLY\s+[A-Za-z0-9-]+\s+BY\s+{var}\b",
                            rf"\bDIVIDE\s+[A-Za-z0-9-]+\s+INTO\s+{var}\b"
                        ]
                        
                        should_skip = False
                        for pattern in skip_patterns:
                            if re.search(pattern, chunk.content, re.IGNORECASE):
                                should_skip = True
                                break
                                
                        if not should_skip:
                            variable_usage.append({"variable": var, "operation": "read"})
                
                if variable_usage:
                    data_flow[chunk.name] = variable_usage
        
        return data_flow
    
    @staticmethod
    def analyze_data_dependencies(chunks):
        """Analyze data dependencies between paragraphs based on variable usage"""
        # First get the data flow for each paragraph
        data_flow = DataFlowAnalyzer.analyze_data_flow(chunks)
        
        # Now identify dependencies between paragraphs
        dependencies = {}
        
        # For each paragraph, find other paragraphs that read variables it writes to
        for para_name, usages in data_flow.items():
            dependencies[para_name] = []
            
            # Find all variables written to in this paragraph
            written_vars = [usage["variable"] for usage in usages if usage["operation"] == "write"]
            
            # For each variable written to, find paragraphs that read it
            for var in written_vars:
                for other_para, other_usages in data_flow.items():
                    if other_para == para_name:
                        continue  # Skip the same paragraph
                        
                    # Check if the other paragraph reads this variable
                    for usage in other_usages:
                        if usage["variable"] == var and usage["operation"] == "read":
                            if other_para not in dependencies[para_name]:
                                dependencies[para_name].append(other_para)
        
        return dependencies
