
from fastapi import APIRouter, HTTPException, Depends
from sqlalchemy.ext.asyncio import AsyncSession
from datetime import datetime
import logging

# Import our modules
from services.training import (
    TrainingDataPreparer,
    start_training_job,
    create_model_version,
    activate_model_version
)
from database import get_db

# Configure logging
logger = logging.getLogger("api-training-routes")

# Create router
router = APIRouter(tags=["training-api"])

@router.post("/training/prepare")
async def prepare_training_data(db: AsyncSession = Depends(get_db)):
    """Prepare training data from feedback"""
    try:
        training_service = TrainingDataPreparer(db)
        result = await training_service.prepare_training_data()
        return result
    except Exception as e:
        logger.error(f"Error preparing training data: {str(e)}", exc_info=True)
        raise HTTPException(
            status_code=500,
            detail=f"Error preparing training data: {str(e)}"
        )

@router.post("/training/start/{job_id}")
async def start_training_job_endpoint(job_id: str, db: AsyncSession = Depends(get_db)):
    """Start a training job"""
    try:
        result = await start_training_job(db, job_id)
        return result
    except Exception as e:
        logger.error(f"Error starting training job: {str(e)}", exc_info=True)
        raise HTTPException(
            status_code=500,
            detail=f"Error starting training job: {str(e)}"
        )

@router.post("/models/version")
async def create_new_model_version(
    model_name: str,
    version: str,
    db: AsyncSession = Depends(get_db)
):
    """Create a new model version entry"""
    try:
        result = await create_model_version(db, model_name, version)
        return result
    except Exception as e:
        logger.error(f"Error creating model version: {str(e)}", exc_info=True)
        raise HTTPException(
            status_code=500,
            detail=f"Error creating model version: {str(e)}"
        )

@router.post("/models/activate/{model_id}")
async def activate_model(model_id: str, db: AsyncSession = Depends(get_db)):
    """Activate a specific model version"""
    try:
        result = await activate_model_version(db, model_id)
        return result
    except Exception as e:
        logger.error(f"Error activating model: {str(e)}", exc_info=True)
        raise HTTPException(
            status_code=500,
            detail=f"Error activating model: {str(e)}"
        )
