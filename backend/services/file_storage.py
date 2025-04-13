
from typing import Dict, Optional
from datetime import datetime

# In-memory storage for uploaded files
# In production, this would use a database
uploaded_files = {}

def store_file(file_id: str, filename: str, content: bytes, lines: int) -> None:
    """Store a file in the in-memory storage."""
    uploaded_files[file_id] = {
        "filename": filename,
        "content": content,
        "size": len(content),
        "lines": lines,
        "timestamp": datetime.now().isoformat()
    }

def get_file(file_id: str) -> Optional[Dict]:
    """Retrieve a file from storage by ID."""
    return uploaded_files.get(file_id)

def file_exists(file_id: str) -> bool:
    """Check if a file exists in storage."""
    return file_id in uploaded_files
