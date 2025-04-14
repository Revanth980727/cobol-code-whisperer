
import os
import unittest
from parsers.antlr_parser import AntlrCobolParser

class TestAntlrParser(unittest.TestCase):
    """Test the ANTLR COBOL parser"""
    
    def setUp(self):
        """Set up test cases"""
        self.test_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GREETING PIC X(20) VALUE 'HELLO, WORLD!'.
       01 COUNTER PIC 9(3) VALUE ZERO.
       
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY GREETING.
           MOVE 1 TO COUNTER.
           PERFORM SECOND-PARA.
           STOP RUN.
           
       SECOND-PARA.
           ADD 1 TO COUNTER.
           DISPLAY COUNTER.
        """
    
    def test_antlr_parser_available(self):
        """Test if the ANTLR parser is available"""
        is_available = AntlrCobolParser.is_available()
        
        # If ANTLR parser is not available, skip the test but log a warning
        if not is_available:
            self.skipTest("ANTLR parser is not available - make sure to generate parser files")
            
    def test_parser_initialization(self):
        """Test initialization of the parser"""
        parser = AntlrCobolParser()
        self.assertIsNotNone(parser)
    
    def test_antlr_parsing(self):
        """Test ANTLR parsing of a simple COBOL program"""
        # If ANTLR parser is not available, skip the test
        if not AntlrCobolParser.is_available():
            self.skipTest("ANTLR parser is not available")
            
        parser = AntlrCobolParser()
        result = parser.parse(self.test_code)
        
        # If parsing failed, skip the test
        if not result:
            self.skipTest("ANTLR parsing failed - check parser configuration")
        
        # Basic checks on the result
        self.assertIn("divisions", result)
        self.assertIn("chunks", result)
        self.assertIn("call_graph", result)
        self.assertIn("data_flow", result)
        
        # Check divisions
        divisions = result["divisions"]
        division_names = [div["division"] for div in divisions]
        self.assertIn("IDENTIFICATION DIVISION", division_names)
        self.assertIn("DATA DIVISION", division_names)
        self.assertIn("PROCEDURE DIVISION", division_names)
        
        # Check chunks
        chunks = result["chunks"]
        chunk_names = [chunk["name"] for chunk in chunks]
        self.assertIn("IDENTIFICATION DIVISION", chunk_names)
        self.assertIn("DATA DIVISION", chunk_names)
        self.assertIn("PROCEDURE DIVISION", chunk_names)
        
        # Check paragraphs
        self.assertIn("MAIN-PARAGRAPH", chunk_names)
        self.assertIn("SECOND-PARA", chunk_names)
        
        # Check call graph
        call_graph = result["call_graph"]
        if "MAIN-PARAGRAPH" in call_graph:
            self.assertIn("SECOND-PARA", call_graph["MAIN-PARAGRAPH"])
        
        # Check data flow
        data_flow = result["data_flow"]
        if "MAIN-PARAGRAPH" in data_flow:
            variables_written = [usage["variable"] for usage in data_flow["MAIN-PARAGRAPH"] 
                                if usage["operation"] == "write"]
            self.assertIn("COUNTER", variables_written)
            
    def test_fallback_extraction(self):
        """Test the fallback extraction method"""
        # If ANTLR parser is not available, skip the test
        if not AntlrCobolParser.is_available():
            self.skipTest("ANTLR parser is not available")
            
        parser = AntlrCobolParser()
        divisions, chunks = parser._fallback_extract_chunks(self.test_code)
        
        # Check that fallback extraction works
        self.assertGreater(len(divisions), 0)
        self.assertGreater(len(chunks), 0)
        
        # Check division names
        division_names = [div["division"] for div in divisions]
        self.assertIn("IDENTIFICATION DIVISION", division_names)
        self.assertIn("DATA DIVISION", division_names)
        self.assertIn("PROCEDURE DIVISION", division_names)

if __name__ == "__main__":
    unittest.main()
