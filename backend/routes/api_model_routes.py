
"""API routes for model operations"""
from fastapi import APIRouter, HTTPException
import logging
from services.llm_service import get_llm_service

# Configure logging
logger = logging.getLogger("api-model-routes")

# Create router
router = APIRouter(prefix="/model-status", tags=["model-api"])

@router.get("/")
async def get_model_status():
    """Get status of the LLM model"""
    llm_service = get_llm_service()
    
    # Check if model is loaded
    is_ready = llm_service.is_ready()
    
    if is_ready:
        # Get model info if available
        model_info = {}
        try:
            model_info = llm_service.model_provider.get_model_info()
        except Exception as e:
            logger.warning(f"Could not get model info: {str(e)}")
            
        return {
            "status": "ready",
            "model_info": model_info
        }
    else:
        return {
            "status": "not_loaded"
        }
