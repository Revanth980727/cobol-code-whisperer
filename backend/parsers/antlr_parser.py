
import os
import logging
from typing import Dict, Any, Optional, List
import importlib.util
from antlr4 import CommonTokenStream, InputStream, ParseTreeWalker
from models.cobol_chunk import CobolChunk
from parsers.chunk_extractor import ChunkExtractor
from parsers.flow_analyzer import ControlFlowAnalyzer
from parsers.data_flow_analyzer import DataFlowAnalyzer

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


class AntlrCobolParser:
    """COBOL parser using ANTLR4 generated parser"""
    
    @classmethod
    def is_available(cls) -> bool:
        """Check if the ANTLR parser is available (generated files exist)"""
        try:
            # Check if the generated ANTLR files exist
            lexer_path = os.path.join(os.path.dirname(__file__), 'antlr_generated', 'Cobol85Lexer.py')
            parser_path = os.path.join(os.path.dirname(__file__), 'antlr_generated', 'Cobol85Parser.py')
            listener_path = os.path.join(os.path.dirname(__file__), 'antlr_generated', 'Cobol85Listener.py')
            
            if not os.path.exists(lexer_path) or not os.path.exists(parser_path):
                logger.warning("ANTLR generated files not found. Using fallback parser.")
                return False
                
            # Check if antlr4 runtime is available
            try:
                import antlr4
                return True
            except ImportError:
                logger.warning("antlr4-python3-runtime not found. Using fallback parser.")
                return False
        except Exception as e:
            logger.warning(f"Error checking ANTLR availability: {str(e)}. Using fallback parser.")
            return False
    
    def __init__(self):
        """Initialize the ANTLR parser"""
        self.use_antlr = self._load_antlr_modules()
        self.divisions = []
        self.chunks = []
    
    def _load_antlr_modules(self):
        """Dynamically load ANTLR generated modules"""
        try:
            # Import the ANTLR4 runtime
            import antlr4
            
            # Import the generated lexer and parser
            try:
                # First, try to import using the standard import mechanism
                from .antlr_generated.Cobol85Lexer import Cobol85Lexer
                from .antlr_generated.Cobol85Parser import Cobol85Parser
                from .antlr_generated.Cobol85Listener import Cobol85Listener
                
                self.Cobol85Lexer = Cobol85Lexer
                self.Cobol85Parser = Cobol85Parser
                self.Cobol85Listener = Cobol85Listener
                logger.info("ANTLR parser loaded successfully")
                return True
            except ImportError:
                # If that fails, try to load the modules dynamically
                logger.warning("Failed to import ANTLR modules directly, trying dynamic import")
                
                # Get the paths to the generated files
                base_dir = os.path.dirname(__file__)
                lexer_path = os.path.join(base_dir, 'antlr_generated', 'Cobol85Lexer.py')
                parser_path = os.path.join(base_dir, 'antlr_generated', 'Cobol85Parser.py')
                listener_path = os.path.join(base_dir, 'antlr_generated', 'Cobol85Listener.py')
                
                if not os.path.exists(lexer_path) or not os.path.exists(parser_path):
                    logger.warning("ANTLR generated files not found")
                    return False
                
                # Load the lexer module
                spec = importlib.util.spec_from_file_location("Cobol85Lexer", lexer_path)
                lexer_module = importlib.util.module_from_spec(spec)
                spec.loader.exec_module(lexer_module)
                self.Cobol85Lexer = lexer_module.Cobol85Lexer
                
                # Load the parser module
                spec = importlib.util.spec_from_file_location("Cobol85Parser", parser_path)
                parser_module = importlib.util.module_from_spec(spec)
                spec.loader.exec_module(parser_module)
                self.Cobol85Parser = parser_module.Cobol85Parser
                
                # Load the listener module
                spec = importlib.util.spec_from_file_location("Cobol85Listener", listener_path)
                listener_module = importlib.util.module_from_spec(spec)
                spec.loader.exec_module(listener_module)
                self.Cobol85Listener = listener_module.Cobol85Listener
                
                logger.info("ANTLR parser loaded successfully (dynamic import)")
                return True
                
        except Exception as e:
            logger.warning(f"Failed to load ANTLR modules: {str(e)}")
            return False
    
    def parse(self, code):
        """Parse COBOL code using ANTLR"""
        if not self.use_antlr:
            logger.warning("ANTLR parser not available")
            return None
            
        try:
            import antlr4
            
            # Create the lexer and parser
            input_stream = antlr4.InputStream(code)
            lexer = self.Cobol85Lexer(input_stream)
            tokens = antlr4.CommonTokenStream(lexer)
            parser = self.Cobol85Parser(tokens)
            
            # Error handling
            parser.removeErrorListeners()
            # You could add a custom error listener here
            
            # Parse the code - Use startRule() method or direct access to parsing rule
            try:
                # First try with the expected startRule method
                parse_tree = parser.startRule()
            except AttributeError:
                # If startRule doesn't exist, try with the compilationUnit rule
                # which is the common entry point for many ANTLR grammars
                try:
                    parse_tree = parser.compilationUnit()
                except AttributeError:
                    # If that fails, try with cobolSource which is specific to COBOL
                    try:
                        parse_tree = parser.cobolSource()
                    except AttributeError:
                        # Final fallback - try checking what the parser's methods are
                        method_names = [method for method in dir(parser) 
                                    if callable(getattr(parser, method)) and 
                                    not method.startswith('_')]
                        logger.info(f"Available parser methods: {method_names}")
                        
                        # Try common rule names for COBOL
                        for rule in ['program', 'programUnit', 'cobol', 'source']:
                            if rule in method_names:
                                parse_tree = getattr(parser, rule)()
                                break
                        else:
                            raise ValueError("Could not find an appropriate start rule for parsing")
            
            logger.info("ANTLR parsing successful, building parse tree")
            
            # Create a custom listener to process the parse tree
            listener = CobolParseTreeListener(parser, code)
            
            # Walk the tree with our listener
            walker = antlr4.ParseTreeWalker()
            walker.walk(listener, parse_tree)
            
            # Get the extracted divisions and chunks
            self.divisions = listener.divisions
            self.chunks = listener.chunks
            
            # If no chunks were extracted, use the fallback method
            if not self.chunks:
                logger.warning("No chunks extracted by ANTLR parser, using fallback extraction")
                self.divisions, self.chunks = self._fallback_extract_chunks(code)
            
            # Build call graph and data flow
            call_graph = listener.call_graph or ControlFlowAnalyzer.build_call_graph(self.chunks)
            data_flow = listener.data_flow or DataFlowAnalyzer.analyze_data_flow(self.chunks)
            
            logger.info(f"ANTLR parser extracted {len(self.chunks)} chunks")
            
            return {
                "divisions": self.divisions,
                "chunks": [chunk.to_dict() for chunk in self.chunks],
                "call_graph": call_graph,
                "data_flow": data_flow
            }
            
        except Exception as e:
            logger.error(f"ANTLR parsing error: {str(e)}")
            return None
    
    def _fallback_extract_chunks(self, code):
        """Fallback method to extract chunks from code if ANTLR parsing fails"""
        from parsers.chunk_extractor import ChunkExtractor
        
        # Split the code into lines
        lines = code.split('\n')
        
        # Create empty divisions and chunks
        divisions = []
        chunks = []
        
        # Extract divisions using regex
        import re
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

