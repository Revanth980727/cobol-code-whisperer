
from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.ext.asyncio import AsyncSession
import logging

# Import our modules
from services.llm_service import get_llm_service
from database import get_db

# Configure logging
logger = logging.getLogger("api-model-routes")

# Create router
router = APIRouter(tags=["model-api"])

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
