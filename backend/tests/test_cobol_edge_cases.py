
import unittest
from backend.parsers.cobol_parser import CobolParser
from backend.analyzers.complexity_analyzer import ComplexityAnalyzer
import os

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
    
    def test_uncommon_syntax(self):
        """Test handling of uncommon COBOL syntax."""
        code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNCOMMON.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TABLE.
          05 WS-ITEM OCCURS 10 TIMES INDEXED BY I
             ASCENDING KEY IS WS-ITEM-ID
             DESCENDING KEY IS WS-ITEM-NAME.
             10 WS-ITEM-ID   PIC 9(5).
             10 WS-ITEM-NAME PIC X(20).
       
       PROCEDURE DIVISION.
           SET I TO 1
           SEARCH ALL WS-ITEM
               AT END
                   DISPLAY "NOT FOUND"
               WHEN WS-ITEM-ID(I) = 12345
                   DISPLAY WS-ITEM-NAME(I)
           END-SEARCH
           
           EVALUATE TRUE
               WHEN A = B AND C = D
                   DISPLAY "CASE 1"
               WHEN A > B OR C > D
                   DISPLAY "CASE 2"
               WHEN OTHER
                   DISPLAY "DEFAULT CASE"
           END-EVALUATE
           
           GOBACK.
        """
        
        result = self.parser.parse_code(code)
        self.assertIsNotNone(result)
    
    def test_sql_embedded(self):
        """Test handling of embedded SQL."""
        code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLTEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ID PIC 9(5).
       01 WS-NAME PIC X(20).
       
       PROCEDURE DIVISION.
           MOVE 12345 TO WS-ID.
           
           EXEC SQL
               SELECT name 
               INTO :WS-NAME
               FROM customers
               WHERE id = :WS-ID
           END-EXEC.
           
           IF SQLCODE = 0
               DISPLAY WS-NAME
           ELSE
               DISPLAY "ERROR: " SQLCODE
           END-IF.
           
           GOBACK.
        """
        
        result = self.parser.parse_code(code)
        self.assertIsNotNone(result)
    
    def test_cics_commands(self):
        """Test handling of CICS commands."""
        code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSTEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CUSTOMER-REC.
          05 CUST-ID   PIC 9(5).
          05 CUST-NAME PIC X(20).
       
       PROCEDURE DIVISION.
           EXEC CICS
               READ DATASET('CUSTFILE')
               INTO(CUSTOMER-REC)
               RIDFLD(CUST-ID)
               RESP(WS-RESP)
           END-EXEC.
           
           IF WS-RESP = DFHRESP(NORMAL)
               DISPLAY CUST-NAME
           ELSE
               EXEC CICS
                   SEND TEXT FROM(ERROR-MSG)
                   ERASE
                   FREEKB
               END-EXEC
           END-IF.
           
           EXEC CICS RETURN END-EXEC.
        """
        
        result = self.parser.parse_code(code)
        self.assertIsNotNone(result)

if __name__ == "__main__":
    unittest.main()
