
# COBOL Code Whisperer Backend

This is the Python backend for the COBOL Code Whisperer application, built with FastAPI.

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

## API Documentation

When the server is running, you can access the auto-generated API documentation at:
- Swagger UI: http://localhost:8000/docs
- ReDoc: http://localhost:8000/redoc

## Endpoints

- `GET /`: Health check
- `POST /analyze-code/`: Upload and analyze a COBOL file
- `POST /feedback/`: Submit feedback on analysis results
- `GET /file/{file_id}`: Retrieve content of a previously uploaded file

## Future Improvements

- Implement actual COBOL parser integration
- Add LLaMA 3 model integration for code analysis
- Implement fine-tuning based on user feedback
- Add database storage for files and feedback
