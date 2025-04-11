
from fastapi import FastAPI, UploadFile, File, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import Optional, List
import os
import uuid

app = FastAPI(title="COBOL Code Whisperer Backend")

# Configure CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:5173", "http://localhost:8080"],  # Frontend URLs
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# In-memory storage for uploaded files and feedback
# In production, this would use a database
uploaded_files = {}
feedback_data = []

class FeedbackModel(BaseModel):
    file_id: str
    rating: int
    comment: Optional[str] = None
    corrected_summary: Optional[str] = None

class AnalysisResponse(BaseModel):
    file_id: str
    summary: str
    business_rules: List[str]
    code_structure: List[dict]
    complexity: dict

@app.get("/")
def read_root():
    return {"status": "COBOL Code Whisperer API is running"}

@app.post("/analyze-code/", response_model=AnalysisResponse)
async def analyze_code(file: UploadFile = File(...)):
    # Save the uploaded file content
    file_id = str(uuid.uuid4())
    file_content = await file.read()
    
    # In production, you'd store this in a secure location
    uploaded_files[file_id] = {
        "filename": file.filename,
        "content": file_content,
        "size": len(file_content),
        "lines": len(file_content.decode('utf-8').splitlines())
    }
    
    # This is where you'd call your COBOL parser and LLaMA model
    # For now, we'll return mock data
    
    # Mock analysis result
    mock_analysis = {
        "file_id": file_id,
        "summary": "This COBOL program processes account transactions. It reads transaction records containing account number, transaction type, and amount.",
        "business_rules": [
            "Transactions must be categorized as deposits or withdrawals",
            "Withdrawal transactions require funds availability check",
            "All transactions are logged for auditing purposes"
        ],
        "code_structure": [
            {
                "division": "IDENTIFICATION DIVISION",
                "elements": [{"name": "PROGRAM-ID", "description": "Transaction Processing Program"}]
            },
            {
                "division": "PROCEDURE DIVISION",
                "elements": [{"name": "MAIN-LOGIC", "description": "Main program flow"}]
            }
        ],
        "complexity": {
            "cyclomatic": 10,
            "linesOfCode": uploaded_files[file_id]["lines"],
            "commentPercentage": 5
        }
    }
    
    return mock_analysis

@app.post("/feedback/")
async def submit_feedback(feedback: FeedbackModel):
    if feedback.file_id not in uploaded_files:
        raise HTTPException(status_code=404, detail="File not found")
    
    # Store the feedback
    feedback_data.append(feedback.dict())
    
    # Here you would trigger or queue the model fine-tuning process
    # based on accumulated feedback
    
    return {"status": "Feedback received", "feedback_id": len(feedback_data)}

@app.get("/file/{file_id}")
async def get_file_content(file_id: str):
    if file_id not in uploaded_files:
        raise HTTPException(status_code=404, detail="File not found")
    
    return {
        "filename": uploaded_files[file_id]["filename"],
        "content": uploaded_files[file_id]["content"].decode('utf-8'),
        "lines": uploaded_files[file_id]["lines"]
    }

if __name__ == "__main__":
    import uvicorn
    uvicorn.run("main:app", host="0.0.0.0", port=8000, reload=True)
