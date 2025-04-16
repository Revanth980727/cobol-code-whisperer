
from fastapi import APIRouter, UploadFile, File, HTTPException, BackgroundTasks, Depends
from sqlalchemy.ext.asyncio import AsyncSession
import logging
import uuid
from typing import Dict, Any
import asyncio

# Import our modules
from services.cobol_service import analyze_cobol_code, get_file_content
from services.db_file_service import store_file_db, get_file_db
from services.llm_service import get_llm_service
from database import get_db
from routes.api import llm_lock  # Import the shared lock

# Configure logging
logger = logging.getLogger("api-file-routes")

# Create router
router = APIRouter(tags=["file-api"])

@router.post("/analyze-code/")
async def analyze_code(
    background_tasks: BackgroundTasks,
    file: UploadFile = File(...),
    db: AsyncSession = Depends(get_db)
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
            # Store the file in the database
            await store_file_db(db, file_id, file.filename, file_content, len(file_content.splitlines()))
            
            # Analyze the code
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

@router.get("/file/{file_id}")
async def get_file_content_handler(file_id: str, db: AsyncSession = Depends(get_db)):
    """Get content of a specific file"""
    try:
        file_data = await get_file_db(db, file_id)
        
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
