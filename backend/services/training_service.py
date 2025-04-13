
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from sqlalchemy import and_
from typing import List, Dict, Any
import logging
import json
import os
from datetime import datetime

from backend.models.database_models import Feedback, TrainingJob, ModelVersion

logger = logging.getLogger("training-service")

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
    
    async def _convert_feedback_to_training_example(self, feedback: Feedback) -> Dict[str, Any]:
        """Convert a feedback entry to a training example."""
        # Skip if no corrected summary (we need the correct output)
        if not feedback.corrected_summary:
            return None
        
        # Get the original file content
        file_result = await self.db.execute(
            select(Feedback.file_id, feedback.file_id)
        )
        file = file_result.fetchone()
        
        if not file:
            logger.warning(f"Original file not found for feedback {feedback.id}")
            return None
        
        # For corrected summaries, we create a prompt-response pair
        training_example = {
            "feedback_id": feedback.id,
            "type": "summary_correction",
            "prompt": f"Summarize the following COBOL code:\n\n{file.content}",
            "completion": feedback.corrected_summary,
            "created_at": feedback.created_at.isoformat() if feedback.created_at else None,
        }
        
        return training_example
    
    async def start_training_job(self, job_id: str) -> Dict[str, Any]:
        """Start a training job."""
        result = await self.db.execute(
            select(TrainingJob).where(TrainingJob.id == job_id)
        )
        job = result.scalars().first()
        
        if not job:
            return {"status": "error", "error": "Job not found"}
        
        if job.status != "ready":
            return {"status": "error", "error": f"Job not in ready state: {job.status}"}
        
        # Update job status
        job.status = "in_progress"
        job.started_at = datetime.now()
        await self.db.commit()
        
        # In a production system, this would trigger an actual training job
        # For this project, we'll simulate it
        
        # This would be the command to run LoRA fine-tuning
        lora_command = """
        python -m backend.lora.train \\
            --model_name "meta-llama/Meta-Llama-3-8B" \\
            --train_file "data/training/training_data_{timestamp}.json" \\
            --output_dir "models/finetuned/llama-3-8b-lora" \\
            --batch_size 4 \\
            --micro_batch_size 1 \\
            --num_epochs 3 \\
            --learning_rate 3e-4 \\
            --cutoff_len 2048 \\
            --val_set_size 0 \\
            --lora_r 8 \\
            --lora_alpha 16 \\
            --lora_dropout 0.05 \\
            --target_modules q_proj,v_proj,k_proj,o_proj 
        """.strip()
        
        logger.info(f"Would execute: {lora_command}")
        
        return {
            "status": "started",
            "job_id": job.id,
            "command": lora_command
        }

async def create_model_version(db: AsyncSession, model_name: str, version: str) -> Dict[str, Any]:
    """Create a new model version entry."""
    model = ModelVersion(
        model_name=model_name,
        version=version,
        is_active=False
    )
    db.add(model)
    await db.commit()
    await db.refresh(model)
    
    return {
        "id": model.id,
        "model_name": model.model_name,
        "version": model.version,
        "created_at": model.created_at.isoformat() if model.created_at else None,
        "is_active": model.is_active
    }

async def activate_model_version(db: AsyncSession, model_id: str) -> Dict[str, Any]:
    """Activate a specific model version and deactivate all others."""
    # First, deactivate all models
    await db.execute(
        "UPDATE model_versions SET is_active = FALSE"
    )
    
    # Then activate the requested model
    result = await db.execute(select(ModelVersion).where(ModelVersion.id == model_id))
    model = result.scalars().first()
    
    if not model:
        return {"status": "error", "error": "Model not found"}
    
    model.is_active = True
    await db.commit()
    
    return {
        "status": "success",
        "model": {
            "id": model.id,
            "model_name": model.model_name,
            "version": model.version,
            "is_active": model.is_active
        }
    }
