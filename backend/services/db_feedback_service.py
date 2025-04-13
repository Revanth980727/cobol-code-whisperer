
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from sqlalchemy import desc
from typing import Dict, List, Optional, Any
import logging
from datetime import datetime

from backend.models.database_models import Feedback, UploadedFile, CodeChunk

logger = logging.getLogger("db-feedback-service")

async def store_feedback_db(
    db: AsyncSession,
    file_id: str,
    rating: int,
    chunk_id: Optional[str] = None,
    comment: Optional[str] = None,
    corrected_summary: Optional[str] = None,
    user_identifier: Optional[str] = None
) -> Optional[str]:
    """Store feedback in the database."""
    # Check if the file exists
    file_result = await db.execute(
        select(UploadedFile.id).where(UploadedFile.id == file_id)
    )
    if not file_result.scalar_one_or_none():
        logger.warning(f"Attempted to add feedback for non-existent file {file_id}")
        return None
    
    # Check if chunk exists if chunk_id is provided
    if chunk_id:
        chunk_result = await db.execute(
            select(CodeChunk.id).where(CodeChunk.id == chunk_id)
        )
        if not chunk_result.scalar_one_or_none():
            logger.warning(f"Attempted to add feedback for non-existent chunk {chunk_id}")
            chunk_id = None  # Set to None if the chunk doesn't exist
    
    # Create and store the feedback
    feedback = Feedback(
        file_id=file_id,
        chunk_id=chunk_id,
        rating=rating,
        comment=comment,
        corrected_summary=corrected_summary,
        user_identifier=user_identifier,
        created_at=datetime.now(),
        is_processed_for_training=False
    )
    
    db.add(feedback)
    await db.commit()
    await db.refresh(feedback)
    
    logger.info(f"Feedback stored successfully: {feedback.id}")
    return feedback.id

async def get_feedback_db(db: AsyncSession, feedback_id: str) -> Optional[Dict]:
    """Retrieve specific feedback by ID."""
    result = await db.execute(select(Feedback).where(Feedback.id == feedback_id))
    feedback = result.scalars().first()
    
    if feedback:
        return {
            "id": feedback.id,
            "file_id": feedback.file_id,
            "chunk_id": feedback.chunk_id,
            "rating": feedback.rating,
            "comment": feedback.comment,
            "corrected_summary": feedback.corrected_summary,
            "user_identifier": feedback.user_identifier,
            "created_at": feedback.created_at.isoformat() if feedback.created_at else None,
            "is_processed_for_training": feedback.is_processed_for_training,
        }
    return None

async def get_all_feedback_db(db: AsyncSession) -> List[Dict[str, Any]]:
    """Retrieve all stored feedback."""
    result = await db.execute(select(Feedback).order_by(desc(Feedback.created_at)))
    feedback_list = result.scalars().all()
    
    return [{
        "id": feedback.id,
        "file_id": feedback.file_id,
        "chunk_id": feedback.chunk_id,
        "rating": feedback.rating,
        "comment": feedback.comment,
        "corrected_summary": feedback.corrected_summary,
        "user_identifier": feedback.user_identifier,
        "created_at": feedback.created_at.isoformat() if feedback.created_at else None,
        "is_processed_for_training": feedback.is_processed_for_training,
    } for feedback in feedback_list]

async def get_feedback_by_file_db(db: AsyncSession, file_id: str) -> List[Dict[str, Any]]:
    """Retrieve all feedback for a specific file."""
    result = await db.execute(
        select(Feedback).where(Feedback.file_id == file_id).order_by(desc(Feedback.created_at))
    )
    feedback_list = result.scalars().all()
    
    return [{
        "id": feedback.id,
        "file_id": feedback.file_id,
        "chunk_id": feedback.chunk_id,
        "rating": feedback.rating,
        "comment": feedback.comment,
        "corrected_summary": feedback.corrected_summary,
        "user_identifier": feedback.user_identifier,
        "created_at": feedback.created_at.isoformat() if feedback.created_at else None,
        "is_processed_for_training": feedback.is_processed_for_training,
    } for feedback in feedback_list]

async def get_feedback_by_chunk_db(db: AsyncSession, chunk_id: str) -> List[Dict[str, Any]]:
    """Retrieve all feedback for a specific code chunk."""
    result = await db.execute(
        select(Feedback).where(Feedback.chunk_id == chunk_id).order_by(desc(Feedback.created_at))
    )
    feedback_list = result.scalars().all()
    
    return [{
        "id": feedback.id,
        "file_id": feedback.file_id,
        "chunk_id": feedback.chunk_id,
        "rating": feedback.rating,
        "comment": feedback.comment,
        "corrected_summary": feedback.corrected_summary,
        "user_identifier": feedback.user_identifier,
        "created_at": feedback.created_at.isoformat() if feedback.created_at else None,
        "is_processed_for_training": feedback.is_processed_for_training,
    } for feedback in feedback_list]

async def mark_feedback_as_processed(db: AsyncSession, feedback_id: str) -> bool:
    """Mark feedback as processed for training."""
    result = await db.execute(select(Feedback).where(Feedback.id == feedback_id))
    feedback = result.scalars().first()
    
    if feedback:
        feedback.is_processed_for_training = True
        await db.commit()
        return True
    return False
