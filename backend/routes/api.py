from fastapi import APIRouter, HTTPException
from fastapi.responses import JSONResponse
from pydantic import BaseModel, Field
from typing import Optional
from datetime import datetime
import logging
import asyncio

# Import our sub-routers
from routes.api_model_routes import router as model_router
from routes.api_file_routes import router as file_router
from routes.api_feedback_routes import router as feedback_router
from routes.api_training_routes import router as training_router

# Configure logging
logger = logging.getLogger("cobol-whisperer-api")

# Create main API router
router = APIRouter(prefix="/api", tags=["cobol-api"])

# Include all sub-routers
router.include_router(model_router)
router.include_router(file_router)
router.include_router(feedback_router)
router.include_router(training_router)

# Concurrency lock for LLM operations - shared across routes
llm_lock = asyncio.Lock()

class ErrorResponse(BaseModel):
    detail: str
    error_code: Optional[str] = None
    timestamp: str = Field(default_factory=lambda: datetime.now().isoformat())

@router.get("/")
def read_root():
    return {"status": "COBOL Code Whisperer API is running"}

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
