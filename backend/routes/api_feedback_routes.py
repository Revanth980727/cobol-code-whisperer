from fastapi import APIRouter, HTTPException, Depends
from sqlalchemy.ext.asyncio import AsyncSession
from pydantic import BaseModel, validator
from typing import Optional
import logging
from datetime import datetime

# Import our modules
from services.db_feedback_service import (
    store_feedback_db,
    get_all_feedback_db,
    get_feedback_by_file_db,
    get_feedback_db
)
from database import get_db

# Configure logging
logger = logging.getLogger("api-feedback-routes")

# Create router
router = APIRouter(tags=["feedback-api"])

# Feedback request model
class FeedbackRequestModel(BaseModel):
    file_id: str
    chunk_id: Optional[str] = None
    rating: int
    comment: Optional[str] = None
    corrected_summary: Optional[str] = None
    user_identifier: Optional[str] = None
    
    @validator('rating')
    def rating_must_be_valid(cls, v):
        if v not in [-1, 0, 1]:
            raise ValueError('Rating must be -1 (negative), 0 (neutral), or 1 (positive)')
        return v

@router.post("/feedback/")
async def submit_feedback(feedback: FeedbackRequestModel, db: AsyncSession = Depends(get_db)):
    """Submit feedback for a specific analysis"""
    try:
        # Store the feedback in the database
        feedback_id = await store_feedback_db(
            db,
            file_id=feedback.file_id,
            rating=feedback.rating,
            chunk_id=feedback.chunk_id,
            comment=feedback.comment,
            corrected_summary=feedback.corrected_summary,
            user_identifier=feedback.user_identifier
        )
        
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
async def get_feedback(db: AsyncSession = Depends(get_db)):
    """Get all stored feedback"""
    try:
        feedback_list = await get_all_feedback_db(db)
        return {"feedback": feedback_list, "count": len(feedback_list)}
    except Exception as e:
        logger.error(f"Error retrieving feedback: {str(e)}", exc_info=True)
        raise HTTPException(
            status_code=500,
            detail=f"Error retrieving feedback: {str(e)}"
        )

@router.get("/feedback/{feedback_id}")
async def get_feedback_by_id(feedback_id: str, db: AsyncSession = Depends(get_db)):
    """Get specific feedback by ID"""
    try:
        feedback = await get_feedback_db(db, feedback_id)
        if not feedback:
            raise HTTPException(status_code=404, detail="Feedback not found")
        return feedback
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Error retrieving feedback: {str(e)}", exc_info=True)
        raise HTTPException(
            status_code=500,
            detail=f"Error retrieving feedback: {str(e)}"
        )

@router.get("/feedback/file/{file_id}")
async def get_feedback_by_file(file_id: str, db: AsyncSession = Depends(get_db)):
    """Get all feedback for a specific file"""
    try:
        feedback_list = await get_feedback_by_file_db(db, file_id)
        return {"feedback": feedback_list, "count": len(feedback_list)}
    except Exception as e:
        logger.error(f"Error retrieving file feedback: {str(e)}", exc_info=True)
        raise HTTPException(
            status_code=500,
            detail=f"Error retrieving file feedback: {str(e)}"
        )
