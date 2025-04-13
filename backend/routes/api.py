
from fastapi import APIRouter, UploadFile, File, HTTPException
from pydantic import BaseModel
from typing import Optional, List, Dict, Any, Union
import uuid
import logging
from datetime import datetime

# Import our modules
from services.cobol_service import analyze_cobol_code, get_file_content
from services.feedback_service import store_feedback

# Configure logging
logger = logging.getLogger("cobol-whisperer-api")

router = APIRouter(prefix="/api", tags=["cobol-api"])

# Model definitions
class FeedbackModel(BaseModel):
    file_id: str
    rating: int
    comment: Optional[str] = None
    corrected_summary: Optional[str] = None


@router.get("/")
def read_root():
    return {"status": "COBOL Code Whisperer API is running"}


@router.get("/model-status")
def model_status():
    """Check if the LLM model is loaded and ready."""
    from services.llm_service import get_llm_service
    llm_service = get_llm_service()
    return {"status": "ready" if llm_service.is_ready() else "not_loaded"}


@router.post("/analyze-code/")
async def analyze_code(file: UploadFile = File(...)):
    # Read the uploaded file
    file_content = await file.read()
    
    # Generate a unique file ID
    file_id = str(uuid.uuid4())
    
    # Analyze the code
    analysis_result = analyze_cobol_code(file_id, file.filename, file_content)
    
    return analysis_result


@router.post("/feedback/")
async def submit_feedback(feedback: FeedbackModel):
    # Store the feedback
    result = store_feedback(feedback)
    
    if not result:
        raise HTTPException(status_code=404, detail="File not found")
    
    return {"status": "Feedback received", "feedback_id": result}


@router.get("/file/{file_id}")
async def get_file_content_handler(file_id: str):
    file_data = get_file_content(file_id)
    
    if not file_data:
        raise HTTPException(status_code=404, detail="File not found")
    
    return file_data
