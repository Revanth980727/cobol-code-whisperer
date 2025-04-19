
from sqlalchemy import Column, String, Integer, Text, Float, DateTime, ForeignKey, Boolean
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func
import uuid
from datetime import datetime

from database import Base

def generate_uuid():
    return str(uuid.uuid4())

class UploadedFile(Base):
    """Database model for uploaded COBOL files"""
    __tablename__ = "uploaded_files"
    
    id = Column(String, primary_key=True, default=generate_uuid)
    filename = Column(String, nullable=False)
    content = Column(Text, nullable=False)  # Store as text for SQLite compatibility
    size = Column(Integer, nullable=False)
    lines = Column(Integer, nullable=False)
    upload_date = Column(DateTime, default=func.now())
    
    # Relationships
    analysis_results = relationship("AnalysisResult", back_populates="file", cascade="all, delete-orphan")
    feedback_entries = relationship("Feedback", back_populates="file", cascade="all, delete-orphan")
    
    def __repr__(self):
        return f"<UploadedFile {self.filename}>"

class AnalysisResult(Base):
    """Database model for analysis results"""
    __tablename__ = "analysis_results"
    
    id = Column(String, primary_key=True, default=generate_uuid)
    file_id = Column(String, ForeignKey("uploaded_files.id", ondelete="CASCADE"), nullable=False)
    summary = Column(Text)
    business_rules = Column(Text)  # Store as JSON string
    complexity_score = Column(Float)
    lines_of_code = Column(Integer)
    comment_percentage = Column(Float)
    cyclomatic_complexity = Column(Float)
    analysis_date = Column(DateTime, default=func.now())
    
    # Relationships
    file = relationship("UploadedFile", back_populates="analysis_results")
    code_chunks = relationship("CodeChunk", back_populates="analysis", cascade="all, delete-orphan")
    
    def __repr__(self):
        return f"<AnalysisResult for file {self.file_id}>"

class CodeChunk(Base):
    """Database model for code chunks"""
    __tablename__ = "code_chunks"
    
    id = Column(String, primary_key=True, default=generate_uuid)
    analysis_id = Column(String, ForeignKey("analysis_results.id", ondelete="CASCADE"), nullable=False)
    chunk_type = Column(String, nullable=False)  # E.g., "PROCEDURE", "DATA", etc.
    name = Column(String, nullable=False)
    content = Column(Text, nullable=False)
    start_line = Column(Integer, nullable=False)
    end_line = Column(Integer, nullable=False)
    line_count = Column(Integer, nullable=False)
    
    # Relationships
    analysis = relationship("AnalysisResult", back_populates="code_chunks")
    feedback_entries = relationship("Feedback", back_populates="code_chunk")
    
    def __repr__(self):
        return f"<CodeChunk {self.name} ({self.chunk_type})>"

class Feedback(Base):
    """Database model for user feedback"""
    __tablename__ = "feedback"
    
    id = Column(String, primary_key=True, default=generate_uuid)
    file_id = Column(String, ForeignKey("uploaded_files.id", ondelete="CASCADE"), nullable=False)
    chunk_id = Column(String, ForeignKey("code_chunks.id", ondelete="SET NULL"), nullable=True)
    rating = Column(Integer, nullable=False)  # -1 (negative), 0 (neutral), 1 (positive)
    comment = Column(Text, nullable=True)
    corrected_summary = Column(Text, nullable=True)
    user_identifier = Column(String, nullable=True)  # Optional user ID or anonymous identifier
    created_at = Column(DateTime, default=func.now())
    is_processed_for_training = Column(Boolean, default=False)
    
    # Relationships
    file = relationship("UploadedFile", back_populates="feedback_entries")
    code_chunk = relationship("CodeChunk", back_populates="feedback_entries")
    
    def __repr__(self):
        return f"<Feedback {self.id} (rating: {self.rating})>"

class ModelVersion(Base):
    """Database model for tracking model versions"""
    __tablename__ = "model_versions"
    
    id = Column(String, primary_key=True, default=generate_uuid)
    model_name = Column(String, nullable=False)
    version = Column(String, nullable=False)
    path = Column(String, nullable=True)  # Path to model weights
    created_at = Column(DateTime, default=func.now())
    is_active = Column(Boolean, default=False)
    
    # Relationships
    training_jobs = relationship("TrainingJob", back_populates="model_version")
    
    def __repr__(self):
        return f"<ModelVersion {self.model_name} v{self.version}>"

class TrainingJob(Base):
    """Database model for training jobs"""
    __tablename__ = "training_jobs"
    
    id = Column(String, primary_key=True, default=generate_uuid)
    feedback_count = Column(Integer, nullable=False)
    status = Column(String, nullable=False)  # "pending", "ready", "in_progress", "completed", "failed"
    training_file = Column(String, nullable=True)  # Path to training data file
    model_version_id = Column(String, ForeignKey("model_versions.id", ondelete="SET NULL"), nullable=True)
    started_at = Column(DateTime, nullable=True)
    completed_at = Column(DateTime, nullable=True)
    error_message = Column(Text, nullable=True)
    
    # Relationships
    model_version = relationship("ModelVersion", back_populates="training_jobs")
    
    def __repr__(self):
        return f"<TrainingJob {self.id} ({self.status})>"
