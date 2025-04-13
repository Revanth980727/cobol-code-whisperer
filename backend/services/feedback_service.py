
from typing import Dict, List, Optional, Any
from datetime import datetime
from pydantic import BaseModel
import logging
import json
import os
import time

# Configure logging
logger = logging.getLogger("feedback-service")

# Constants
FEEDBACK_DIR = os.environ.get("FEEDBACK_DIR", "data/feedback")

# Create feedback directory if it doesn't exist
os.makedirs(FEEDBACK_DIR, exist_ok=True)

class FeedbackModel(BaseModel):
    """Model for representing feedback"""
    file_id: str
    chunk_id: Optional[str] = None
    rating: int  # -1 for negative, 0 for neutral, 1 for positive
    comment: Optional[str] = None
    corrected_summary: Optional[str] = None
    timestamp: str = None

# In-memory cache for quick access
feedback_cache = {}

def store_feedback(feedback: FeedbackModel) -> Optional[str]:
    """Store feedback in the file system and in-memory cache."""
    from services.file_storage import file_exists
    
    # Check if the file exists
    if not file_exists(feedback.file_id):
        logger.warning(f"Attempted to add feedback for non-existent file {feedback.file_id}")
        return None
    
    # Generate a unique feedback ID
    feedback_id = f"feedback_{int(time.time())}_{feedback.file_id}"
    
    # Ensure timestamp is set
    if feedback.timestamp is None:
        feedback.timestamp = datetime.now().isoformat()
    
    # Store the feedback
    feedback_dict = feedback.dict()
    
    # Add to in-memory cache
    feedback_cache[feedback_id] = feedback_dict
    
    # Write to filesystem for persistence
    try:
        feedback_file_path = os.path.join(FEEDBACK_DIR, f"{feedback_id}.json")
        with open(feedback_file_path, "w") as f:
            json.dump(feedback_dict, f, indent=2)
        logger.info(f"Feedback stored successfully: {feedback_id}")
    except Exception as e:
        logger.error(f"Error saving feedback to file: {str(e)}", exc_info=True)
        # Continue anyway since we have it in memory
    
    return feedback_id

def get_feedback(feedback_id: str) -> Optional[Dict]:
    """Retrieve specific feedback by ID."""
    # Check in-memory cache first
    if feedback_id in feedback_cache:
        return feedback_cache[feedback_id]
    
    # Try to load from file
    try:
        feedback_file_path = os.path.join(FEEDBACK_DIR, f"{feedback_id}.json")
        if os.path.exists(feedback_file_path):
            with open(feedback_file_path, "r") as f:
                feedback_data = json.load(f)
                # Update cache
                feedback_cache[feedback_id] = feedback_data
                return feedback_data
    except Exception as e:
        logger.error(f"Error loading feedback {feedback_id}: {str(e)}", exc_info=True)
    
    return None

def get_all_feedback() -> List[Dict[str, Any]]:
    """Retrieve all stored feedback."""
    all_feedback = []
    
    # Load from files to ensure we have everything
    try:
        for filename in os.listdir(FEEDBACK_DIR):
            if filename.endswith(".json"):
                feedback_id = filename[:-5]  # Remove .json extension
                try:
                    with open(os.path.join(FEEDBACK_DIR, filename), "r") as f:
                        feedback_data = json.load(f)
                        # Update cache
                        feedback_cache[feedback_id] = feedback_data
                        all_feedback.append(feedback_data)
                except Exception as e:
                    logger.error(f"Error loading feedback file {filename}: {str(e)}", exc_info=True)
    except Exception as e:
        logger.error(f"Error listing feedback directory: {str(e)}", exc_info=True)
    
    return all_feedback

def get_feedback_by_file(file_id: str) -> List[Dict[str, Any]]:
    """Retrieve all feedback for a specific file."""
    return [f for f in get_all_feedback() if f.get("file_id") == file_id]

def get_feedback_by_chunk(chunk_id: str) -> List[Dict[str, Any]]:
    """Retrieve all feedback for a specific code chunk."""
    return [f for f in get_all_feedback() if f.get("chunk_id") == chunk_id]
