
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from typing import Dict, Any, Optional
import logging
import json
import os
from pathlib import Path
from datetime import datetime

from backend.models.database_models import Feedback, TrainingJob, UploadedFile, CodeChunk

logger = logging.getLogger("training-data-preparer")

class TrainingDataPreparer:
    """Service for preparing training data from feedback."""
    
    def __init__(self, db: AsyncSession):
        self.db = db
        # Directory for saving training data
        self.training_dir = os.environ.get("TRAINING_DATA_DIR", "data/training")
        os.makedirs(self.training_dir, exist_ok=True)
    
    async def prepare_training_data(self) -> Dict[str, Any]:
        """Prepare training data from unprocessed feedback."""
        # Get unprocessed feedback
        result = await self.db.execute(
            select(Feedback).where(Feedback.is_processed_for_training == False)
        )
        feedback_list = result.scalars().all()
        
        if not feedback_list:
            logger.info("No new feedback to process for training")
            return {"status": "no_data", "count": 0}
        
        # Create a new training job
        job = TrainingJob(
            feedback_count=len(feedback_list),
            status="pending",
        )
        self.db.add(job)
        await self.db.flush()
        
        # Prepare data for lora fine-tuning
        try:
            training_data = []
            
            for feedback in feedback_list:
                # Convert feedback to training example
                training_example = await self._convert_feedback_to_training_example(feedback)
                if training_example:
                    training_data.append(training_example)
                    
                    # Mark as processed
                    feedback.is_processed_for_training = True
            
            # Save training data to file
            if training_data:
                timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
                file_path = os.path.join(self.training_dir, f"training_data_{timestamp}.json")
                with open(file_path, "w") as f:
                    json.dump(training_data, f, indent=2)
                
                job.status = "ready"
                job.training_file = file_path
                logger.info(f"Training data prepared: {len(training_data)} examples saved to {file_path}")
            else:
                job.status = "no_data"
                logger.info("No valid training examples could be created")
            
            await self.db.commit()
            
            return {
                "status": job.status,
                "job_id": job.id,
                "count": len(training_data),
                "file_path": file_path if training_data else None,
            }
            
        except Exception as e:
            logger.error(f"Error preparing training data: {str(e)}", exc_info=True)
            job.status = "failed"
            job.error_message = str(e)
            await self.db.commit()
            return {"status": "error", "error": str(e)}
    
    async def _convert_feedback_to_training_example(self, feedback: Feedback) -> Optional[Dict[str, Any]]:
        """Convert a feedback entry to a training example."""
        # Skip if no corrected summary (we need the correct output)
        if not feedback.corrected_summary:
            return None
        
        # Get the original file content
        file_result = await self.db.execute(
            select(UploadedFile).where(UploadedFile.id == feedback.file_id)
        )
        file = file_result.scalars().first()
        
        if not file:
            logger.warning(f"Original file not found for feedback {feedback.id}")
            return None
            
        # If we have a code chunk, use that specific content
        chunk_content = None
        if feedback.chunk_id:
            chunk_result = await self.db.execute(
                select(CodeChunk).where(CodeChunk.id == feedback.chunk_id)
            )
            chunk = chunk_result.scalars().first()
            if chunk:
                chunk_content = chunk.content
                chunk_type = chunk.chunk_type
                chunk_name = chunk.name
        
        # Create the training example with the appropriate content and context
        if chunk_content:
            # For specific code chunk feedback
            prompt = f"Summarize the following COBOL {chunk_type} '{chunk_name}':\n\n{chunk_content}"
        else:
            # For full file feedback
            prompt = f"Summarize the following COBOL code:\n\n{file.content}"
        
        training_example = {
            "feedback_id": feedback.id,
            "type": "summary_correction",
            "prompt": prompt,
            "completion": feedback.corrected_summary,
            "rating": feedback.rating,
            "created_at": feedback.created_at.isoformat() if feedback.created_at else None,
        }
        
        return training_example
