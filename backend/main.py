
from fastapi import FastAPI, Request
from fastapi.middleware.cors import CORSMiddleware
import logging
import uvicorn
import asyncio
import os
from pathlib import Path

# Import our API routes and database components
from routes.api import router as api_router
from services.llm_service import get_llm_service
from database import engine, Base

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[logging.StreamHandler()]
)
logger = logging.getLogger("cobol-whisperer")

# Ensure data directories exist
os.makedirs("data/files", exist_ok=True)
os.makedirs("data/feedback", exist_ok=True)
os.makedirs("data/training", exist_ok=True)

# Initialize FastAPI app
app = FastAPI(
    title="COBOL Code Whisperer Backend",
    description="API for analyzing COBOL code and generating documentation",
    version="0.1.0"
)

# Configure CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:5173", "http://localhost:8080"],  # Frontend URLs
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Include our routers - include without a prefix since routes already have /api prefix
app.include_router(api_router)

# Add middleware for tracking requests
@app.middleware("http")
async def log_requests(request: Request, call_next):
    path = request.url.path
    method = request.method
    logger.info(f"Request: {method} {path}")
    
    # Process the request
    response = await call_next(request)
    
    # Log completion
    logger.info(f"Response: {method} {path} - Status: {response.status_code}")
    return response

# Startup event to initialize services
@app.on_event("startup")
async def startup_event():
    """Initialize services on startup"""
    logger.info("Starting COBOL Code Whisperer Backend")
    
    # Initialize the LLM service
    try:
        llm_service = get_llm_service()
        # Initialize model in background to avoid blocking startup
        asyncio.create_task(llm_service.model_provider.initialize_model())
        logger.info("LLM service initialization started")
    except Exception as e:
        logger.error(f"Error initializing LLM service: {str(e)}", exc_info=True)
        logger.warning("Application will continue without LLM capabilities")

# Root endpoint
@app.get("/")
def read_root():
    return {"status": "COBOL Code Whisperer API is running"}

# Health check endpoint
@app.get("/health")
async def health_check():
    # Check if the LLM is loaded
    llm_service = get_llm_service()
    llm_status = "ready" if llm_service.is_ready() else "not_loaded"
    
    # Return health status
    return {
        "status": "healthy",
        "llm_status": llm_status,
        "version": "0.1.0"
    }

if __name__ == "__main__":
    uvicorn.run("main:app", host="0.0.0.0", port=8000, reload=True)
