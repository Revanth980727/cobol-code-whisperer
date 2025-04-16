
from fastapi import APIRouter, Depends
from services.llm_service import get_llm_service
import logging

# Configure logging
logger = logging.getLogger("api-model-routes")

# Create router
router = APIRouter(tags=["model-api"])

@router.get("/model-status")
async def get_model_status():
    """Check if the LLM model is loaded and ready"""
    llm_service = get_llm_service()
    is_ready = llm_service.is_ready()
    
    model_status = {
        "status": "ready" if is_ready else "not_loaded"
    }
    
    # Add model info if available
    if is_ready and hasattr(llm_service.model_provider, 'model_info'):
        model_status["model_info"] = llm_service.model_provider.model_info
    
    return model_status
