
from fastapi import APIRouter, UploadFile, File, HTTPException, BackgroundTasks, Depends
from pydantic import BaseModel, Field, validator
from typing import Optional, List, Dict, Any, Union
import uuid
import logging
from datetime import datetime
import asyncio
from fastapi.responses import JSONResponse

# Import our modules
from services.cobol_service import analyze_cobol_code, get_file_content
from services.feedback_service import store_feedback, get_all_feedback, FeedbackModel
from services.llm_service import get_llm_service

# Configure logging
logger = logging.getLogger("cobol-whisperer-api")

router = APIRouter(prefix="/api", tags=["cobol-api"])

# Concurrency lock for LLM operations
llm_lock = asyncio.Lock()

# Enhanced model definitions
class FeedbackRequestModel(BaseModel):
    file_id: str
    chunk_id: Optional[str] = None
    rating: int
    comment: Optional[str] = None
    corrected_summary: Optional[str] = None
    
    @validator('rating')
    def rating_must_be_valid(cls, v):
        if v not in [-1, 0, 1]:
            raise ValueError('Rating must be -1 (negative), 0 (neutral), or 1 (positive)')
        return v

class ErrorResponse(BaseModel):
    detail: str
    error_code: Optional[str] = None
    timestamp: str = Field(default_factory=lambda: datetime.now().isoformat())


@router.get("/")
def read_root():
    return {"status": "COBOL Code Whisperer API is running"}


@router.get("/model-status")
async def model_status():
    """Check if the LLM model is loaded and ready."""
    try:
        llm_service = get_llm_service()
        is_ready = llm_service.is_ready()
        
        # Include model information if available
        model_info = llm_service.model_provider.get_model_info() if is_ready else {}
        
        return {
            "status": "ready" if is_ready else "not_loaded",
            "model_info": model_info
        }
    except Exception as e:
        logger.error(f"Error checking model status: {str(e)}")
        raise HTTPException(
            status_code=500, 
            detail=f"Error checking model status: {str(e)}"
        )


@router.post("/analyze-code/")
async def analyze_code(
    background_tasks: BackgroundTasks,
    file: UploadFile = File(...)
):
    """Analyze uploaded COBOL code"""
    try:
        # Validate file content
        if file.filename is None or file.filename == "":
            raise HTTPException(status_code=400, detail="No filename provided")
            
        # Check file type (simple check for common COBOL extensions)
        valid_extensions = ['.cob', '.cbl', '.cpy', '.cobol', '.txt']
        if not any(file.filename.lower().endswith(ext) for ext in valid_extensions):
            logger.warning(f"Potentially invalid COBOL file: {file.filename}")
            # Continue but log warning - some users may have unusual extensions
        
        # Read the uploaded file
        file_content = await file.read()
        
        if len(file_content) == 0:
            raise HTTPException(status_code=400, detail="Empty file")
            
        if len(file_content) > 5 * 1024 * 1024:  # 5MB limit
            raise HTTPException(status_code=400, detail="File too large (max 5MB)")
        
        # Generate a unique file ID
        file_id = str(uuid.uuid4())
        
        # Analyze the code
        async with llm_lock:
            analysis_result = analyze_cobol_code(file_id, file.filename, file_content)
        
        return analysis_result
        
    except HTTPException:
        # Re-raise HTTP exceptions
        raise
    except Exception as e:
        logger.error(f"Error analyzing code: {str(e)}", exc_info=True)
        raise HTTPException(
            status_code=500,
            detail=f"Error analyzing code: {str(e)}"
        )


@router.post("/feedback/")
async def submit_feedback(feedback: FeedbackRequestModel):
    """Submit feedback for a specific analysis"""
    try:
        # Convert to our internal model
        feedback_model = FeedbackModel(
            file_id=feedback.file_id,
            chunk_id=feedback.chunk_id,
            rating=feedback.rating,
            comment=feedback.comment,
            corrected_summary=feedback.corrected_summary,
            timestamp=datetime.now().isoformat()
        )
        
        # Store the feedback
        feedback_id = store_feedback(feedback_model)
        
        if not feedback_id:
            raise HTTPException(status_code=404, detail="File not found")
        
        return {
            "status": "Feedback received", 
            "feedback_id": feedback_id,
            "timestamp": datetime.now().isoformat()
        }
        
    except HTTPException:
        # Re-raise HTTP exceptions
        raise
    except Exception as e:
        logger.error(f"Error storing feedback: {str(e)}", exc_info=True)
        raise HTTPException(
            status_code=500,
            detail=f"Error storing feedback: {str(e)}"
        )


@router.get("/feedback/")
async def get_feedback():
    """Get all stored feedback"""
    try:
        feedback_list = get_all_feedback()
        return {"feedback": feedback_list, "count": len(feedback_list)}
    except Exception as e:
        logger.error(f"Error retrieving feedback: {str(e)}", exc_info=True)
        raise HTTPException(
            status_code=500,
            detail=f"Error retrieving feedback: {str(e)}"
        )


@router.get("/file/{file_id}")
async def get_file_content_handler(file_id: str):
    """Get content of a specific file"""
    try:
        file_data = get_file_content(file_id)
        
        if not file_data:
            raise HTTPException(status_code=404, detail="File not found")
        
        return file_data
        
    except HTTPException:
        # Re-raise HTTP exceptions
        raise
    except Exception as e:
        logger.error(f"Error retrieving file: {str(e)}", exc_info=True)
        raise HTTPException(
            status_code=500,
            detail=f"Error retrieving file: {str(e)}"
        )


@router.exception_handler(HTTPException)
async def http_exception_handler(request, exc):
    """Custom exception handler for HTTP exceptions"""
    return JSONResponse(
        status_code=exc.status_code,
        content=ErrorResponse(
            detail=exc.detail,
            error_code=f"ERR_{exc.status_code}"
        ).dict()
    )
