
from fastapi import APIRouter, Depends
from services.llm_service import get_llm_service
import logging

# Configure logging
logger = logging.getLogger("api-model-routes")

# Create router
router = APIRouter(prefix="", tags=["model-api"])  # Remove prefix to match expected route structure

@router.get("/api/model-status")  # Update the route to include /api prefix
async def get_model_status():
    """Check if the LLM model is loaded and ready"""
    llm_service = get_llm_service()
    is_ready = llm_service.is_ready()
    
    model_status = {
        "status": "ready" if is_ready else "loading",
        "model_name": None,
        "context_length": None,
        "device": None
    }
    
    # Add model info if available
    if is_ready and hasattr(llm_service.model_provider, 'get_model_info'):
        try:
            model_info = llm_service.model_provider.get_model_info()
            model_status["model_name"] = model_info.get("name")
            model_status["context_length"] = model_info.get("context_length", 4096)
            model_status["device"] = model_info.get("device", "CPU")
            model_status["quantization"] = model_info.get("quantization")
        except Exception as e:
            logger.error(f"Error getting model info: {str(e)}")
            model_status["status"] = "error"
            model_status["error"] = str(e)
    
    return model_status
