
from typing import Dict, List, Optional
from datetime import datetime
from pydantic import BaseModel

# In-memory storage for feedback
# In production, this would use a database
feedback_data = []

class FeedbackModel(BaseModel):
    file_id: str
    rating: int
    comment: Optional[str] = None
    corrected_summary: Optional[str] = None

def store_feedback(feedback: FeedbackModel) -> Optional[int]:
    """Store feedback in the in-memory storage."""
    from services.file_storage import file_exists
    
    # Check if the file exists
    if not file_exists(feedback.file_id):
        return None
    
    # Store the feedback with timestamp
    feedback_dict = feedback.dict()
    feedback_dict["timestamp"] = datetime.now().isoformat()
    feedback_data.append(feedback_dict)
    
    return len(feedback_data)
