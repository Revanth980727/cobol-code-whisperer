
# COBOL Code Whisperer Backend

FastAPI backend for the COBOL Code Whisperer application.

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

3. Run the server:
```bash
uvicorn main:app --reload
```

The API will be available at http://localhost:8000

## Endpoints

- `GET /`: Health check
- `POST /analyze-code/`: Upload and analyze a COBOL file
- `POST /feedback/`: Submit feedback on analysis results
- `GET /file/{file_id}`: Retrieve content of a previously uploaded file
