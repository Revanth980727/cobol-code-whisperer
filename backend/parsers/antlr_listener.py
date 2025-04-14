
import logging
from typing import Dict, Any, List
from models.cobol_chunk import CobolChunk

# Configure logging
logger = logging.getLogger("cobol_parser")

class CobolParseTreeListener:
    """Listener for ANTLR parse tree events"""
    
    def __init__(self, parser, code_text):
        self.parser = parser
        self.code_text = code_text
        self.divisions = []
        self.chunks = []
        self.current_division = None
        self.current_section = None
        self.variable_definitions = {}
        self.call_graph = {}
        self.data_flow = {}
    
    def enterIdentificationDivision(self, ctx):
        self._process_division("IDENTIFICATION DIVISION", ctx)
    
    def enterEnvironmentDivision(self, ctx):
        self._process_division("ENVIRONMENT DIVISION", ctx)
    
    def enterDataDivision(self, ctx):
        self._process_division("DATA DIVISION", ctx)
    
    def enterProcedureDivision(self, ctx):
        self._process_division("PROCEDURE DIVISION", ctx)
    
    def enterParagraph(self, ctx):
        if not ctx.paragraphName() or not ctx.paragraphName().getText():
            return
        
        # Get paragraph name and content
        para_name = ctx.paragraphName().getText().upper()
        start_token = ctx.start.start
        end_token = ctx.stop.stop if ctx.stop else len(self.code_text) - 1
        
        # Get the line numbers
        start_line = ctx.start.line
        end_line = ctx.stop.line if ctx.stop else start_line + self.code_text[start_token:end_token].count('\n')
        
        # Get the paragraph content
        para_content = self.code_text[start_token:end_token + 1]
        
        # Create a chunk for the paragraph
        paragraph_chunk = CobolChunk(
            "paragraph",
            para_name,
            para_content,
            start_line,
            end_line
        )
        
        # Add the paragraph to the chunks list
        self.chunks.append(paragraph_chunk)
        
        # Add the paragraph to the current division
        if self.current_division is not None:
            division_idx = next((i for i, d in enumerate(self.divisions) 
                              if d["division"] == self.current_division), None)
            if division_idx is not None:
                if "paragraphs" not in self.divisions[division_idx]:
                    self.divisions[division_idx]["paragraphs"] = []
                
                self.divisions[division_idx]["paragraphs"].append({
                    "name": para_name,
                    "line": start_line
                })
        
        # Track perform statements for call graph
        self._extract_perform_statements(para_content, para_name)
        
        # Track data operations for data flow
        self._extract_data_operations(para_content, para_name)
    
    def enterSection(self, ctx):
        if not ctx.sectionName() or not ctx.sectionName().getText():
            return
            
        # Get section name and content
        section_name = ctx.sectionName().getText().upper()
        start_token = ctx.start.start
        end_token = ctx.stop.stop if ctx.stop else len(self.code_text) - 1
        
        # Get the line numbers
        start_line = ctx.start.line
        end_line = ctx.stop.line if ctx.stop else start_line + self.code_text[start_token:end_token].count('\n')
        
        # Get the section content
        section_content = self.code_text[start_token:end_token + 1]
        
        # Create a chunk for the section
        section_chunk = CobolChunk(
            "section",
            section_name,
            section_content,
            start_line,
            end_line
        )
        
        # Add the section to the chunks list
        self.chunks.append(section_chunk)
        
        # Set the current section
        self.current_section = section_name
        
        # Add the section to the current division
        if self.current_division is not None:
            division_idx = next((i for i, d in enumerate(self.divisions) 
                              if d["division"] == self.current_division), None)
            if division_idx is not None:
                if "sections" not in self.divisions[division_idx]:
                    self.divisions[division_idx]["sections"] = []
                
                self.divisions[division_idx]["sections"].append({
                    "name": section_name,
                    "line": start_line
                })
    
    def enterDataItem(self, ctx):
        """Process data items for working-storage and linkage sections"""
        try:
            # This method will depend on the ANTLR grammar's structure
            # for data item definition nodes
            if hasattr(ctx, 'dataName') and ctx.dataName():
                data_name = ctx.dataName().getText().upper()
                level_number = ctx.levelNumber().getText() if hasattr(ctx, 'levelNumber') else "01"
                
                # Store variable definition
                self.variable_definitions[data_name] = {
                    "level": level_number,
                    "section": self.current_section,
                    "division": self.current_division
                }
        except Exception as e:
            logger.debug(f"Error processing data item: {e}")
    
    def _process_division(self, division_name, ctx):
        """Process a division node"""
        # Get the start and end positions for the division
        start_token = ctx.start.start
        end_token = ctx.stop.stop if ctx.stop else len(self.code_text) - 1
        
        # Get the line numbers
        start_line = ctx.start.line
        end_line = ctx.stop.line if ctx.stop else start_line + self.code_text[start_token:end_token].count('\n')
        
        # Get the division content
        division_text = self.code_text[start_token:end_token + 1]
        
        # Create a chunk for the division
        division_chunk = CobolChunk(
            "division",
            division_name,
            division_text,
            start_line,
            end_line
        )
        
        # Add the division to the chunks list
        self.chunks.append(division_chunk)
        
        # Add the division to the divisions list
        self.divisions.append({
            "division": division_name,
            "line": start_line,
            "elements": []
        })
        
        # Set the current division
        self.current_division = division_name
    
    def _extract_perform_statements(self, content, paragraph_name):
        """Extract PERFORM statements to build call graph"""
        import re
        perform_pattern = r'\bPERFORM\s+([A-Z0-9-]+)'
        
        for match in re.finditer(perform_pattern, content, re.IGNORECASE):
            target_para = match.group(1).upper()
            
            if paragraph_name not in self.call_graph:
                self.call_graph[paragraph_name] = []
                
            if target_para not in self.call_graph[paragraph_name]:
                self.call_graph[paragraph_name].append(target_para)
    
    def _extract_data_operations(self, content, paragraph_name):
        """Extract data operations for data flow analysis"""
        import re
        
        # Track variable reads
        read_pattern = r'\b(?:USING|IF|WHEN|UNTIL|VARYING)\s+([A-Z0-9-]+)'
        # Track variable writes
        write_pattern = r'\b(?:MOVE|COMPUTE|SET|ADD|SUBTRACT|MULTIPLY|DIVIDE)(?:\s+.+\s+TO\s+|\s+.+\s+GIVING\s+)([A-Z0-9-]+)'
        
        if paragraph_name not in self.data_flow:
            self.data_flow[paragraph_name] = []
        
        # Process reads
        for match in re.finditer(read_pattern, content, re.IGNORECASE):
            variable = match.group(1).upper()
            if variable in self.variable_definitions:
                self.data_flow[paragraph_name].append({
                    "variable": variable,
                    "operation": "read"
                })
        
        # Process writes
        for match in re.finditer(write_pattern, content, re.IGNORECASE):
            variable = match.group(1).upper()
            if variable in self.variable_definitions:
                self.data_flow[paragraph_name].append({
                    "variable": variable,
                    "operation": "write"
                })
