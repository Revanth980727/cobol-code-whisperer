
import unittest
import os
from parsers.antlr_parser import AntlrCobolParser
from cobol_parser import CobolParser

class TestIntegratedParser(unittest.TestCase):
    """Test the integrated COBOL parser with ANTLR"""
    
    def setUp(self):
        """Set up test cases"""
        self.test_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INTEGRATED-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TEST-VAR PIC X(10) VALUE 'TEST'.
       01 COUNTER  PIC 9(5) VALUE ZERO.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY 'STARTING TEST'.
           MOVE 100 TO COUNTER.
           PERFORM PROCESS-DATA.
           STOP RUN.
           
       PROCESS-DATA.
           ADD 1 TO COUNTER.
           IF COUNTER > 200
               DISPLAY 'COUNTER IS HIGH: ' COUNTER
           ELSE
               DISPLAY 'COUNTER IS LOW: ' COUNTER
           END-IF.
        """
        
        # Save environment variable to restore it later
        self.original_use_antlr = os.environ.get('USE_ANTLR')
    
    def tearDown(self):
        """Restore environment variables"""
        if self.original_use_antlr is not None:
            os.environ['USE_ANTLR'] = self.original_use_antlr
        elif 'USE_ANTLR' in os.environ:
            del os.environ['USE_ANTLR']
    
    def test_integrated_parsing_with_antlr(self):
        """Test integrated parsing with ANTLR enabled"""
        # Set environment variable
        os.environ['USE_ANTLR'] = 'true'
        
        # If ANTLR parser is not available, skip the test
        if not AntlrCobolParser.is_available():
            self.skipTest("ANTLR parser is not available")
        
        # Create parser instance
        parser = CobolParser(self.test_code)
        
        # Check if ANTLR is available in the parser
        self.assertEqual(parser.antlr_parser_available, AntlrCobolParser.is_available())
        
        # Parse the code
        result = parser.parse_code(self.test_code, use_antlr=True)
        
        # Basic checks
        self.assertIsNotNone(result)
        self.assertIn("divisions", result)
        self.assertIn("chunks", result)
        self.assertIn("call_graph", result)
        self.assertIn("data_flow", result)
        self.assertIn("MAIN-LOGIC", [chunk["name"] for chunk in result["chunks"]])
    
    def test_integrated_parsing_without_antlr(self):
        """Test integrated parsing with ANTLR disabled"""
        # Set environment variable
        os.environ['USE_ANTLR'] = 'false'
        
        # Create parser instance
        parser = CobolParser(self.test_code)
        
        # Parse the code with ANTLR disabled
        result = parser.parse_code(self.test_code, use_antlr=False)
        
        # Basic checks
        self.assertIsNotNone(result)
        self.assertIn("divisions", result)
        self.assertIn("chunks", result)
        self.assertIn("call_graph", result)
        self.assertIn("data_flow", result)
        self.assertIn("MAIN-LOGIC", [chunk["name"] for chunk in result["chunks"]])
    
    def test_fallback_parsing(self):
        """Test fallback parsing when ANTLR fails"""
        # Create a parser with invalid COBOL code that will cause the ANTLR parser to fail
        invalid_code = """
        THIS IS NOT VALID COBOL SYNTAX
        BUT THE FALLBACK PARSER SHOULD STILL HANDLE IT
        """
        
        parser = CobolParser()
        
        # Parse with fallback enabled
        result = parser.parse_code(invalid_code, use_fallback=True)
        
        # Check that we got some result
        self.assertIsNotNone(result)
        
        # It will likely use the fallback parser which returns different fields
        if "warnings" in result:
            self.assertIn("Used fallback parser", result["warnings"][0])

if __name__ == "__main__":
    unittest.main()
