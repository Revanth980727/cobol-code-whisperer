
# ANTLR COBOL85 Grammar

This directory should contain the COBOL85 grammar files (.g4) from the ANTLR grammar repository.

## Required Files

1. `Cobol85Lexer.g4`
2. `Cobol85Parser.g4`
3. `Cobol85PreprocessorParser.g4` (optional for handling COPY/REPLACE statements)

## How to Get the Grammar Files

Download the grammar files from: https://github.com/antlr/grammars-v4/tree/master/cobol85

```bash
# Download the grammar files
mkdir -p backend/parsers/antlr_grammar
cd backend/parsers/antlr_grammar

# Download the required files
curl -O https://raw.githubusercontent.com/antlr/grammars-v4/master/cobol85/Cobol85.g4
curl -O https://raw.githubusercontent.com/antlr/grammars-v4/master/cobol85/Cobol85Lexer.g4
curl -O https://raw.githubusercontent.com/antlr/grammars-v4/master/cobol85/Cobol85Parser.g4
```

## Generating Parser Files

After placing the grammar files in this directory, run the ANTLR tool to generate the parser:

```bash
cd backend
mkdir -p parsers/antlr_generated
java -jar /path/to/antlr-4.13.1-complete.jar -Dlanguage=Python3 -visitor parsers/antlr_grammar/Cobol85Lexer.g4 parsers/antlr_grammar/Cobol85Parser.g4 -o parsers/antlr_generated
```

### Alternative Method Using Docker

If you have Docker installed, you can use the ANTLR tool without installing Java:

```bash
cd backend
docker run --rm -v $(pwd):/work -w /work antlr/antlr4:4.13.1 -Dlanguage=Python3 -visitor parsers/antlr_grammar/Cobol85Lexer.g4 parsers/antlr_grammar/Cobol85Parser.g4 -o parsers/antlr_generated
```

## Testing the Parser Generation

To verify if the parser is correctly generated, you can run:

```bash
cd backend
python -c "from parsers.antlr_parser import AntlrCobolParser; print(f'ANTLR parser available: {AntlrCobolParser.is_available()}')"
```

## Important Files Generated

The ANTLR tool will generate several Python files:
- `Cobol85Lexer.py` - The lexer that tokenizes the COBOL code
- `Cobol85Parser.py` - The parser that builds the parse tree
- `Cobol85Listener.py` - The listener interface for traversing the parse tree
- `Cobol85Visitor.py` - The visitor interface for traversing the parse tree

## Usage in the Application

After the parser files are generated, the system will automatically detect and use them if available. If they're not available, it will fall back to the regex-based parser.

You can control whether to use ANTLR by setting the environment variable:

```bash
# To enable ANTLR parser (default)
export USE_ANTLR=true

# To disable ANTLR parser and use only the standard parser
export USE_ANTLR=false
```
