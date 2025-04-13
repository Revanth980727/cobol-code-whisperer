
# This directory contains the ANTLR-generated Python files
# When ANTLR processes the grammar files, it will generate:
# - Cobol85Lexer.py
# - Cobol85Parser.py
# - Cobol85Listener.py
# - Cobol85Visitor.py (if visitor pattern is used)

# Make the generated modules easier to import
try:
    from .Cobol85Lexer import Cobol85Lexer
    from .Cobol85Parser import Cobol85Parser
    from .Cobol85Listener import Cobol85Listener
    # Add Visitor if it exists
    try:
        from .Cobol85Visitor import Cobol85Visitor
    except ImportError:
        pass
except ImportError:
    # Generated files may not exist yet
    pass
