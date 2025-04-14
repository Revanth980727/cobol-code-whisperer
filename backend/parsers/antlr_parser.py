
import os
import logging
from typing import Dict, Any, Optional, List
from antlr4 import CommonTokenStream, InputStream, ParseTreeWalker
from models.cobol_chunk import CobolChunk
from parsers.flow_analyzer import ControlFlowAnalyzer
from parsers.data_flow_analyzer import DataFlowAnalyzer
from parsers.antlr_listener import CobolParseTreeListener
from parsers.antlr_module_loader import AntlrModuleLoader
from parsers.antlr_fallback import AntlrFallbackExtractor

# Configure logging
logger = logging.getLogger("cobol_parser")

class AntlrCobolParser:
    """COBOL parser using ANTLR4 generated parser"""
    
    @classmethod
    def is_available(cls) -> bool:
        """Check if the ANTLR parser is available (generated files exist)"""
        return AntlrModuleLoader.check_antlr_availability()
    
    def __init__(self):
        """Initialize the ANTLR parser"""
        success, lexer_class, parser_class, listener_class = AntlrModuleLoader.load_antlr_modules()
        self.use_antlr = success
        self.Cobol85Lexer = lexer_class
        self.Cobol85Parser = parser_class
        self.Cobol85Listener = listener_class
        self.divisions = []
        self.chunks = []
    
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
                self.divisions, self.chunks = AntlrFallbackExtractor.extract_chunks(code)
            
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
        return AntlrFallbackExtractor.extract_chunks(code)
