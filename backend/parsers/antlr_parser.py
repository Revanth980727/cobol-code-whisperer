
import os
import logging
from typing import Dict, Any, Optional
import importlib.util

# Configure logging
logger = logging.getLogger("cobol_parser")

class AntlrCobolParser:
    """COBOL parser using ANTLR4 generated parser"""
    
    @classmethod
    def is_available(cls) -> bool:
        """Check if the ANTLR parser is available (generated files exist)"""
        try:
            # Check if the generated ANTLR files exist
            lexer_path = os.path.join(os.path.dirname(__file__), 'antlr_generated', 'Cobol85Lexer.py')
            parser_path = os.path.join(os.path.dirname(__file__), 'antlr_generated', 'Cobol85Parser.py')
            
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
                self.Cobol85Lexer = Cobol85Lexer
                self.Cobol85Parser = Cobol85Parser
                logger.info("ANTLR parser loaded successfully")
                return True
            except ImportError:
                # If that fails, try to load the modules dynamically
                logger.warning("Failed to import ANTLR modules directly, trying dynamic import")
                
                # Get the paths to the generated files
                base_dir = os.path.dirname(__file__)
                lexer_path = os.path.join(base_dir, 'antlr_generated', 'Cobol85Lexer.py')
                parser_path = os.path.join(base_dir, 'antlr_generated', 'Cobol85Parser.py')
                
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
            from models.cobol_chunk import CobolChunk
            
            # Create the lexer and parser
            input_stream = antlr4.InputStream(code)
            lexer = self.Cobol85Lexer(input_stream)
            tokens = antlr4.CommonTokenStream(lexer)
            parser = self.Cobol85Parser(tokens)
            
            # Parse the code
            parse_tree = parser.startRule()  # Adjust the start rule based on the grammar
            
            # Extract divisions, sections, and paragraphs using a visitor or listener
            # This requires extending antlr4.ParseTreeVisitor or antlr4.ParseTreeListener
            # For now, we'll use a simple approach to extract divisions
            
            self.divisions = []
            self.chunks = []
            
            # Extract IDENTIFICATION DIVISION
            if hasattr(parse_tree, 'identificationDivision'):
                id_div = parse_tree.identificationDivision()
                if id_div:
                    self._extract_division("IDENTIFICATION DIVISION", id_div, code)
            
            # Extract ENVIRONMENT DIVISION
            if hasattr(parse_tree, 'environmentDivision'):
                env_div = parse_tree.environmentDivision()
                if env_div:
                    self._extract_division("ENVIRONMENT DIVISION", env_div, code)
            
            # Extract DATA DIVISION
            if hasattr(parse_tree, 'dataDivision'):
                data_div = parse_tree.dataDivision()
                if data_div:
                    self._extract_division("DATA DIVISION", data_div, code)
            
            # Extract PROCEDURE DIVISION
            if hasattr(parse_tree, 'procedureDivision'):
                proc_div = parse_tree.procedureDivision()
                if proc_div:
                    self._extract_division("PROCEDURE DIVISION", proc_div, code)
                    self._extract_sections_and_paragraphs(proc_div, code)
            
            # Build call graph and data flow
            from parsers.flow_analyzer import ControlFlowAnalyzer
            from parsers.data_flow_analyzer import DataFlowAnalyzer
            
            call_graph = ControlFlowAnalyzer.build_call_graph(self.chunks)
            data_flow = DataFlowAnalyzer.analyze_data_flow(self.chunks)
            
            return {
                "divisions": self.divisions,
                "chunks": [chunk.to_dict() for chunk in self.chunks],
                "call_graph": call_graph,
                "data_flow": data_flow
            }
            
        except Exception as e:
            logger.error(f"ANTLR parsing error: {str(e)}")
            return None
    
    def _extract_division(self, division_name, division_ctx, code):
        """Extract a division from the parse tree"""
        # Get the start and end positions for the division
        start_token = division_ctx.start.start
        end_token = division_ctx.stop.stop
        
        # Get the line numbers
        start_line = division_ctx.start.line
        end_line = division_ctx.stop.line
        
        # Get the text for the division
        division_text = code[start_token:end_token + 1]
        
        # Create a chunk for the division
        from models.cobol_chunk import CobolChunk
        division_chunk = CobolChunk(
            "division",
            division_name,
            division_text,
            start_line,
            end_line
        )
        
        # Add the division to the list
        self.chunks.append(division_chunk)
        
        # Add the division to the divisions list
        self.divisions.append({
            "division": division_name,
            "line": start_line,
            "elements": []
        })
    
    def _extract_sections_and_paragraphs(self, procedure_div_ctx, code):
        """Extract sections and paragraphs from the procedure division"""
        # This would require a more detailed visitor or listener implementation
        # For now, we'll use a placeholder implementation
        
        # In a complete implementation, we would:
        # 1. Iterate through the sections in the procedure division
        # 2. For each section, extract its name and content
        # 3. For each section, iterate through its paragraphs
        # 4. For each paragraph, extract its name and content
        
        # Placeholder implementation - use regex-based extraction as fallback
        from parsers.chunk_extractor import ChunkExtractor
        
        # Find the index of the procedure division in the chunks list
        procedure_div_index = -1
        for i, chunk in enumerate(self.chunks):
            if chunk.chunk_type == "division" and "PROCEDURE DIVISION" in chunk.name:
                procedure_div_index = i
                break
        
        if procedure_div_index >= 0:
            # Extract sections and paragraphs using the ChunkExtractor
            # Note: This is a fallback method when detailed ANTLR extraction is not implemented
            self.divisions, self.chunks = ChunkExtractor.extract_sections_and_paragraphs(
                self.divisions, self.chunks, code.split('\n'))
