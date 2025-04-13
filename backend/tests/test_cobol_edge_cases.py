
import unittest
from backend.parsers.cobol_parser import CobolParser
from backend.analyzers.complexity_analyzer import ComplexityAnalyzer
import os
import re

class TestCobolEdgeCases(unittest.TestCase):
    """Test cases for edge COBOL syntax."""
    
    def setUp(self):
        self.parser = CobolParser()
        self.complexity_analyzer = ComplexityAnalyzer()
        
        # Create a directory for test files if it doesn't exist
        self.test_files_dir = os.path.join(os.path.dirname(__file__), "test_files")
        os.makedirs(self.test_files_dir, exist_ok=True)
    
    def test_empty_divisions(self):
        """Test handling of empty divisions."""
        code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPTYDIV.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       
       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD'.
           STOP RUN.
        """
        
        result = self.parser.parse_code(code)
        self.assertIsNotNone(result)
        self.assertEqual(result["program_id"], "EMPTYDIV")
    
    def test_missing_sections(self):
        """Test handling of missing required sections."""
        code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MISSINGSECTIONS.
       
       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD'.
           STOP RUN.
        """
        
        result = self.parser.parse_code(code)
        self.assertIsNotNone(result)
        self.assertEqual(result["program_id"], "MISSINGSECTIONS")
    
    def test_complex_nested_if(self):
        """Test handling of deeply nested IF statements."""
        code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NESTEDIF.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 A PIC 9(2) VALUE 5.
       01 B PIC 9(2) VALUE 10.
       01 C PIC 9(2) VALUE 15.
       01 D PIC 9(2) VALUE 20.
       01 RESULT PIC 9(2) VALUE 0.
       
       PROCEDURE DIVISION.
           IF A < B
               IF B < C
                   IF C < D
                       MOVE 1 TO RESULT
                   ELSE
                       IF A < D
                           MOVE 2 TO RESULT
                       ELSE
                           MOVE 3 TO RESULT
                       END-IF
                   END-IF
               ELSE
                   IF A < C
                       IF B < D
                           MOVE 4 TO RESULT
                       ELSE
                           MOVE 5 TO RESULT
                       END-IF
                   ELSE
                       MOVE 6 TO RESULT
                   END-IF
               END-IF
           ELSE
               MOVE 7 TO RESULT
           END-IF.
           
           DISPLAY RESULT.
           STOP RUN.
        """
        
        result = self.complexity_analyzer.analyze(code)
        self.assertGreater(result["cyclomatic_complexity"], 5)
    
    def test_copybook_inclusion(self):
        """Test handling of COPY statements."""
        # First, create a copybook file
        copybook_path = os.path.join(self.test_files_dir, "TESTCOPY.cpy")
        with open(copybook_path, "w") as f:
            f.write("""
       01 COPIED-DATA.
          05 COPY-FIELD-A PIC X(10).
          05 COPY-FIELD-B PIC 9(5).
            """)
        
        # Now test a program that includes it
        code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COPYTEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY TESTCOPY.
       
       PROCEDURE DIVISION.
           MOVE "TEST" TO COPY-FIELD-A.
           DISPLAY COPY-FIELD-A.
           STOP RUN.
        """
        
        # Save this code to a file for testing
        code_path = os.path.join(self.test_files_dir, "copytest.cbl")
        with open(code_path, "w") as f:
            f.write(code)
        
        # Test that we can parse programs with COPY statements
        with open(code_path, "r") as f:
            code_content = f.read()
            result = self.parser.parse_code(code_content)
            self.assertIsNotNone(result)
            
    def test_replace_statement(self):
        """Test handling of REPLACE statements."""
        code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPLACETEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MY-VAR PIC X(10).
       
       PROCEDURE DIVISION.
           REPLACE ==MY-VAR== BY ==CUSTOMER-NAME==.
           MOVE "JOHN DOE" TO MY-VAR.
           DISPLAY MY-VAR.
           STOP RUN.
        """
        
        result = self.parser.parse_code(code)
        self.assertIsNotNone(result)
        # Check if the REPLACE statement is properly identified
        self.assertTrue(any("REPLACE" in str(div).upper() for div in result["divisions"]))
    
    def test_perform_thru(self):
        """Test handling of PERFORM THRU statements."""
        code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFORMTHRU.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 COUNTER PIC 9(3) VALUE 0.
       
       PROCEDURE DIVISION.
           PERFORM PARA-1 THRU PARA-3.
           STOP RUN.
           
       PARA-1.
           ADD 1 TO COUNTER.
           
       PARA-2.
           ADD 2 TO COUNTER.
           
       PARA-3.
           ADD 3 TO COUNTER.
           DISPLAY COUNTER.
        """
        
        result = self.parser.parse_code(code)
        self.assertIsNotNone(result)
        
        # Check if all paragraphs are detected
        paragraphs = [p["name"] for p in result["paragraphs"]]
        self.assertIn("PARA-1", paragraphs)
        self.assertIn("PARA-2", paragraphs)
        self.assertIn("PARA-3", paragraphs)
        
    def test_goto_statement(self):
        """Test handling of GO TO statements."""
        code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GOTOTEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 COUNTER PIC 9(3) VALUE 0.
       
       PROCEDURE DIVISION.
           MOVE 1 TO COUNTER.
           GO TO PARA-CHECK.
           
       PARA-UNREACHABLE.
           MOVE 999 TO COUNTER.
           
       PARA-CHECK.
           IF COUNTER < 5
               ADD 1 TO COUNTER
               GO TO PARA-CHECK
           END-IF.
           DISPLAY COUNTER.
           STOP RUN.
        """
        
        result = self.parser.parse_code(code)
        self.assertIsNotNone(result)
        
        # Check if control flow with GO TO is detected
        control_flow = self.parser.extract_control_flow(code)
        self.assertTrue(any("GO TO" in cf for cf in control_flow))
    
    def test_inspect_string_unstring(self):
        """Test handling of INSPECT, STRING, and UNSTRING statements."""
        code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRINGOPS.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SOURCE-TEXT     PIC X(50) VALUE "This is a test string with spaces".
       01 TARGET-TEXT     PIC X(50) VALUE SPACES.
       01 WORD-ARRAY.
          05 WORD OCCURS 10 TIMES PIC X(15).
       01 DELIMITER-IN    PIC X VALUE SPACE.
       01 SPACE-COUNT     PIC 99 VALUE ZERO.
       
       PROCEDURE DIVISION.
           INSPECT SOURCE-TEXT
               TALLYING SPACE-COUNT FOR ALL SPACES.
               
           STRING "Word count: " DELIMITED BY SIZE
                  SPACE-COUNT   DELIMITED BY SIZE
                  INTO TARGET-TEXT.
               
           UNSTRING SOURCE-TEXT
               DELIMITED BY SPACE
               INTO WORD(1) WORD(2) WORD(3) WORD(4) WORD(5)
                   WORD(6) WORD(7) WORD(8) WORD(9) WORD(10).
               
           DISPLAY TARGET-TEXT.
           DISPLAY WORD(1) ", " WORD(2) ", " WORD(3).
           STOP RUN.
        """
        
        result = self.parser.parse_code(code)
        self.assertIsNotNone(result)
        
        # Check if string operations are detected
        self.assertTrue(self.parser.contains_verb(code, "INSPECT"))
        self.assertTrue(self.parser.contains_verb(code, "STRING"))
        self.assertTrue(self.parser.contains_verb(code, "UNSTRING"))
    
    def test_dialect_variations(self):
        """Test handling of dialect-specific variations."""
        
        # IBM mainframe style
        ibm_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. IBMDIALECT.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-VARIABLES.
           05  WS-NUM      PIC S9(4) COMP.
           05  WS-CHAR     PIC X(10).
           
       PROCEDURE DIVISION.
           MOVE 100 TO WS-NUM
           MOVE 'IBM STYLE' TO WS-CHAR
           DISPLAY WS-CHAR ' ' WS-NUM
           GOBACK.
        """
        
        # Microfocus style
        mf_code = """
       $SET DIALECT"MF"
       identification division.
       program-id. mfdialect.
       
       data division.
       working-storage section.
       01  ws-variables.
           05  ws-num      pic s9(4) comp-5.
           05  ws-char     pic x(10).
           
       procedure division.
           move 100 to ws-num
           move "MF style" to ws-char
           display ws-char space ws-num
           stop run.
        """
        
        # Test both dialects
        ibm_result = self.parser.parse_code(ibm_code)
        mf_result = self.parser.parse_code(mf_code)
        
        self.assertIsNotNone(ibm_result)
        self.assertIsNotNone(mf_result)
        self.assertEqual(ibm_result["program_id"], "IBMDIALECT")
        self.assertEqual(mf_result["program_id"], "MFDIALECT")
        
    def test_parser_fallback(self):
        """Test parser fallback mechanism for problematic code."""
        # Intentionally malformed code
        bad_code = """
       IDENTIFICATION DIVISION
       PROGRAM-ID. BADCODE.
       
       PROCEDURE DIVISION
           DISPLAY 'Missing period after division'
           STOP RUN.
        """
        
        # Test fallback parsing
        result = self.parser.parse_code(bad_code, use_fallback=True)
        self.assertIsNotNone(result)
        self.assertTrue(result["warnings"])  # Should have warnings
        
    def test_contains_verb(self):
        """Helper method to detect COBOL verbs in code"""
        code = """
           MOVE "Test" TO OUTPUT-FIELD.
           DISPLAY OUTPUT-FIELD.
        """
        self.assertTrue(self.parser.contains_verb(code, "MOVE"))
        self.assertTrue(self.parser.contains_verb(code, "DISPLAY"))
        self.assertFalse(self.parser.contains_verb(code, "COMPUTE"))
        
    def extract_control_flow(self, code):
        """Helper method to extract control flow statements"""
        flow_statements = []
        # Look for GO TO statements
        for match in re.finditer(r'\bGO\s+TO\s+([A-Za-z0-9-]+)', code, re.IGNORECASE):
            flow_statements.append(f"GO TO {match.group(1)}")
        
        # Look for PERFORM statements
        for match in re.finditer(r'\bPERFORM\s+([A-Za-z0-9-]+)(?:\s+THRU\s+([A-Za-z0-9-]+))?', 
                                code, re.IGNORECASE):
            if match.group(2):
                flow_statements.append(f"PERFORM {match.group(1)} THRU {match.group(2)}")
            else:
                flow_statements.append(f"PERFORM {match.group(1)}")
                
        return flow_statements

if __name__ == "__main__":
    unittest.main()
