
import logging
import re
from typing import List, Dict, Any, Optional
from parsers.cobol_parser import CobolParser as BaseCobolParser
from analyzers.complexity_analyzer import calculate_complexity

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("cobol_parser")

class CobolParser(BaseCobolParser):
    """Enhanced COBOL parser with additional features for edge cases"""
    
    def __init__(self, code=None):
        super().__init__(code) if code else super().__init__()
        self.dialects = {
            "IBM": r"\bCOMP\b|\bGOBACK\b|\bGO\s+TO\b|\bALTER\b|\bPERFORM\s+.*\s+THRU\b",
            "MF": r"\$SET\s+DIALECT|COMP-5|\bSTOP\s+RUN\b|\bPERFORM\s+.*\s+THROUGH\b",
            "ACUCOBOL": r"\$SET\s+OKFORAC|\bACU\b|\bSCREEN\s+SECTION\b",
            "GNU": r"\bCOBOL\s+85\b|\bANSI\b"
        }
        
        # For tracking copy statements
        self.copy_modules = {}
        # For tracking flow control
        self.control_flow_graph = {}
    
    def parse_code(self, code, use_fallback=False):
        """Parse COBOL code with fallback mechanisms"""
        try:
            # Preprocess COPY and REPLACE statements first
            preprocessed_code = self.preprocess_code(code)
            
            # Detect dialect for adaptive parsing
            dialect = self.detect_dialect(preprocessed_code)
            logger.info(f"Detected COBOL dialect: {dialect}")
            
            # Parse the preprocessed code
            result = self.parse(preprocessed_code)
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
    
    def preprocess_code(self, code):
        """Preprocess COBOL code to handle COPY and REPLACE statements"""
        # Process COPY statements
        copy_pattern = r'COPY\s+([A-Za-z0-9-]+)(?:\s+REPLACING\s+(.+?))?.'
        
        # First, identify all COPY modules
        for match in re.finditer(copy_pattern, code, re.IGNORECASE | re.DOTALL):
            module_name = match.group(1)
            self.copy_modules[module_name] = {
                "position": match.start(),
                "length": match.end() - match.start(),
                "replacements": match.group(2) if match.group(2) else None
            }
            
        # TODO: In a real implementation, we would resolve COPY statements by:
        # 1. Looking up the copybook content from a repository
        # 2. Applying any REPLACING clauses
        # 3. Inserting the processed content into the code
        
        # Process REPLACE statements
        replace_pattern = r'REPLACE\s+(.+?)\s+BY\s+(.+?).'
        
        processed_code = code
        for match in re.finditer(replace_pattern, code, re.IGNORECASE | re.DOTALL):
            old_text = match.group(1).strip()
            new_text = match.group(2).strip()
            
            # Strip pseudo-text delimiters if present
            if old_text.startswith('==') and old_text.endswith('=='):
                old_text = old_text[2:-2].strip()
            if new_text.startswith('==') and new_text.endswith('=='):
                new_text = new_text[2:-2].strip()
                
            # Apply replacement to the rest of the code after this REPLACE statement
            start_pos = match.end()
            before_replace = processed_code[:start_pos]
            after_replace = processed_code[start_pos:]
            after_replace = after_replace.replace(old_text, new_text)
            processed_code = before_replace + after_replace
            
        return processed_code
    
    def fallback_parse(self, code):
        """Simple fallback parser for when the main parser fails"""
        # Basic structure extraction
        divisions = []
        program_id = None
        warnings = ["Used fallback parser due to syntax issues"]
        
        # Try to extract program ID
        program_id_match = re.search(r'PROGRAM-ID\.\s+([A-Za-z0-9-]+)', code, re.IGNORECASE)
        if program_id_match:
            program_id = program_id_match.group(1)
        
        # Try to identify divisions
        for div in ["IDENTIFICATION", "ENVIRONMENT", "DATA", "PROCEDURE"]:
            if re.search(rf'{div}\s+DIVISION', code, re.IGNORECASE):
                divisions.append(f"{div} DIVISION")
        
        # Basic paragraph extraction
        paragraphs = []
        para_pattern = r'^\s*([A-Za-z0-9-]+)\s*\.\s*$'
        for match in re.finditer(para_pattern, code, re.MULTILINE):
            para_name = match.group(1).upper()
            # Skip if this is likely not a paragraph (common keywords)
            if para_name not in ["PROGRAM-ID", "AUTHOR", "DATE-WRITTEN", "SECURITY"]:
                paragraphs.append({"name": para_name, "line": match.start()})
        
        # Line-by-line fallback parsing
        lines = code.split("\n")
        line_analysis = []
        for i, line in enumerate(lines):
            line_type = "unknown"
            content = line.strip()
            
            if not content:
                line_type = "blank"
            elif content.startswith('*'):
                line_type = "comment"
            elif re.search(r'\bPERFORM\b', content, re.IGNORECASE):
                line_type = "perform"
            elif re.search(r'\bGO\s+TO\b', content, re.IGNORECASE):
                line_type = "goto"
            elif re.search(r'\bIF\b', content, re.IGNORECASE):
                line_type = "condition"
            elif re.search(r'\bMOVE\b', content, re.IGNORECASE):
                line_type = "move"
            elif re.search(r'\bDISPLAY\b', content, re.IGNORECASE):
                line_type = "display"
                
            line_analysis.append({"line_num": i+1, "content": content, "type": line_type})
        
        # Detect dialect
        dialect = self.detect_dialect(code)
        
        return {
            "program_id": program_id,
            "divisions": divisions,
            "paragraphs": paragraphs,
            "line_analysis": line_analysis,
            "dialect": dialect,
            "warnings": warnings
        }
    
    def detect_dialect(self, code):
        """Detect COBOL dialect based on syntax patterns"""
        dialect_scores = {}
        
        for dialect, pattern in self.dialects.items():
            matches = len(re.findall(pattern, code, re.IGNORECASE))
            dialect_scores[dialect] = matches
        
        if not any(dialect_scores.values()):
            return "UNKNOWN"
        
        return max(dialect_scores, key=dialect_scores.get)
    
    def contains_verb(self, code, verb):
        """Check if code contains a specific COBOL verb"""
        pattern = rf'\b{verb}\b'
        return bool(re.search(pattern, code, re.IGNORECASE))
    
    def extract_control_flow(self, code):
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
    
    def analyze_control_flow(self, code):
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
