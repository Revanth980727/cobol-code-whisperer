
import os
import logging
import importlib.util
from typing import Tuple, Any, Optional

# Configure logging
logger = logging.getLogger("cobol_parser")

class AntlrModuleLoader:
    """Helper class for dynamically loading ANTLR modules"""
    
    @classmethod
    def load_antlr_modules(cls) -> Tuple[bool, Any, Any, Any]:
        """
        Load the ANTLR generated modules
        
        Returns:
            Tuple of (success, Cobol85Lexer, Cobol85Parser, Cobol85Listener)
        """
        try:
            # Import the ANTLR4 runtime
            import antlr4
            
            # Try to import using the standard import mechanism
            try:
                from .antlr_generated.Cobol85Lexer import Cobol85Lexer
                from .antlr_generated.Cobol85Parser import Cobol85Parser
                from .antlr_generated.Cobol85Listener import Cobol85Listener
                
                logger.info("ANTLR parser loaded successfully")
                return True, Cobol85Lexer, Cobol85Parser, Cobol85Listener
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
                    return False, None, None, None
                
                # Load the lexer module
                spec = importlib.util.spec_from_file_location("Cobol85Lexer", lexer_path)
                lexer_module = importlib.util.module_from_spec(spec)
                spec.loader.exec_module(lexer_module)
                Cobol85Lexer = lexer_module.Cobol85Lexer
                
                # Load the parser module
                spec = importlib.util.spec_from_file_location("Cobol85Parser", parser_path)
                parser_module = importlib.util.module_from_spec(spec)
                spec.loader.exec_module(parser_module)
                Cobol85Parser = parser_module.Cobol85Parser
                
                # Load the listener module
                spec = importlib.util.spec_from_file_location("Cobol85Listener", listener_path)
                listener_module = importlib.util.module_from_spec(spec)
                spec.loader.exec_module(listener_module)
                Cobol85Listener = listener_module.Cobol85Listener
                
                logger.info("ANTLR parser loaded successfully (dynamic import)")
                return True, Cobol85Lexer, Cobol85Parser, Cobol85Listener
                
        except Exception as e:
            logger.warning(f"Failed to load ANTLR modules: {str(e)}")
            return False, None, None, None
    
    @classmethod
    def check_antlr_availability(cls) -> bool:
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
