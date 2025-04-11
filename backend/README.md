
# COBOL Code Whisperer Backend

FastAPI backend for the COBOL Code Whisperer application with LLaMA 3 integration.

## Setup

1. Create a virtual environment:
```bash
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
```

2. Install dependencies:
```bash
pip install -r requirements.txt
```

3. Setup the LLaMA 3 model:

   **Option 1: Using HuggingFace Transformers (default)**
   
   The default configuration uses HuggingFace Transformers to load the model. If you have a GPU with enough memory, you can use this option.
   
   Note: You need to have access to Meta's LLaMA 3 model on HuggingFace or specify a different model.

   **Option 2: Using llama.cpp (optimized for CPU)**
   
   For better CPU performance or limited resources, set the environment variable:
   ```bash
   export USE_LLAMA_CPP=true
   export LLAMA_MODEL_PATH=/path/to/your/model.gguf
   ```
   
   You'll need to download a GGUF version of LLaMA 3 for this option.

4. Run the server:
```bash
uvicorn main:app --reload
```

The API will be available at http://localhost:8000

## Endpoints

- `GET /`: Health check
- `GET /model-status`: Check if the LLM model is loaded and ready
- `POST /analyze-code/`: Upload and analyze a COBOL file
- `POST /feedback/`: Submit feedback on analysis results
- `GET /file/{file_id}`: Retrieve content of a previously uploaded file
