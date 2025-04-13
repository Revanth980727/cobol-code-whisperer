
# COBOL Code Whisperer Backend

FastAPI backend for the COBOL Code Whisperer application with LLaMA integration and SQLite database.

## Features

- COBOL code parsing and analysis
  - Lightweight regex-based parser
  - Grammar-based parsing using ANTLR (optional)
- LLaMA integration for AI-powered code understanding
- SQLite + SQLAlchemy for persistent storage
- Feedback collection and storage
- LoRA-ready training script for model fine-tuning
- Docker support for easy deployment

## Setup

### Option 1: Using Docker (Recommended)

1. Build and start the services:
```bash
docker-compose up --build
```

The API will be available at http://localhost:8000

### Option 2: Manual Setup

1. Create a virtual environment:
```bash
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
```

2. Install dependencies:
```bash
pip install -r requirements.txt
```

3. Run database migrations:
```bash
cd backend
alembic upgrade head
```

4. Setup the LLaMA model:

   **Option A: Using HuggingFace Transformers (default)**
   
   The default configuration uses HuggingFace Transformers to load the model.
   
   Note: You need to have access to Meta's LLaMA model on HuggingFace or specify a different model.

   **Option B: Using llama.cpp (optimized for CPU)**
   
   For better CPU performance or limited resources, set the environment variable:
   ```bash
   export USE_LLAMA_CPP=true
   export LLAMA_MODEL_PATH=/path/to/your/model.gguf
   ```
   
   You'll need to download a GGUF version of LLaMA for this option.

5. Run the server:
```bash
uvicorn main:app --reload
```

The API will be available at http://localhost:8000

## ANTLR COBOL85 Grammar Integration

The application supports grammar-based parsing using ANTLR4 with the COBOL85 grammar. This provides a more accurate parsing of COBOL code, especially for complex syntax and edge cases.

### Setting up ANTLR4 for COBOL85 parsing

1. **Download the ANTLR4 tool**:
   
   Download the ANTLR4 JAR file from [the ANTLR website](https://www.antlr.org/download.html):
   ```bash
   wget https://www.antlr.org/download/antlr-4.13.1-complete.jar
   ```

2. **Download the COBOL85 grammar files**:
   
   Download the grammar files from [the ANTLR grammars-v4 repository](https://github.com/antlr/grammars-v4/tree/master/cobol85):
   ```bash
   mkdir -p backend/parsers/antlr_grammar
   # Download Cobol85Lexer.g4, Cobol85Parser.g4, etc.
   ```

3. **Generate the ANTLR4 parser**:
   
   ```bash
   mkdir -p backend/parsers/antlr_generated
   java -jar antlr-4.13.1-complete.jar -Dlanguage=Python3 \
        -o backend/parsers/antlr_generated \
        backend/parsers/antlr_grammar/Cobol85Lexer.g4 \
        backend/parsers/antlr_grammar/Cobol85Parser.g4
   ```

4. **Restart the application**:
   
   The application will automatically detect and use the ANTLR parser if available.

### Notes on ANTLR Parser Usage

- The ANTLR parser is used by default if available, with fallback to the regex-based parser.
- For large or complex COBOL programs, the ANTLR parser provides better accuracy and more detailed analysis.
- The ANTLR parser integrates with the existing data flow and control flow analysis.

## Database Structure

The application uses SQLAlchemy with SQLite to store:

- Uploaded COBOL files
- Analysis results and code chunks
- User feedback
- Model versions and training jobs

## Feedback for Fine-Tuning

The application collects structured feedback that can be used for fine-tuning:

1. User ratings (positive/negative/neutral)
2. Text corrections or suggestions
3. Corrected summaries

## Training Pipeline

1. Prepare training data:
   ```bash
   curl -X POST http://localhost:8000/api/training/prepare
   ```

2. Start a training job:
   ```bash
   curl -X POST http://localhost:8000/api/training/start/{job_id}
   ```

## API Endpoints

### Code Analysis
- `GET /`: Health check
- `GET /health`: Detailed health check
- `GET /api/model-status`: Check if the LLM model is loaded and ready
- `POST /api/analyze-code/`: Upload and analyze a COBOL file

### Feedback
- `POST /api/feedback/`: Submit feedback on analysis results
- `GET /api/feedback/`: Get all stored feedback
- `GET /api/feedback/{feedback_id}`: Get specific feedback by ID
- `GET /api/feedback/file/{file_id}`: Get all feedback for a file

### File Management
- `GET /api/file/{file_id}`: Retrieve content of a previously uploaded file

### Training
- `POST /api/training/prepare`: Prepare training data from feedback
- `POST /api/training/start/{job_id}`: Start a training job
- `POST /api/models/version`: Create a new model version
- `POST /api/models/activate/{model_id}`: Activate a specific model version
