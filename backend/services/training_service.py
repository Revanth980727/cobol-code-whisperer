
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from sqlalchemy import and_
from typing import List, Dict, Any, Optional
import logging
import json
import os
import subprocess
from pathlib import Path
from datetime import datetime

from backend.models.database_models import Feedback, TrainingJob, ModelVersion, UploadedFile, CodeChunk

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
        
        # Get the training file path
        if not job.training_file:
            job.status = "failed"
            job.error_message = "No training file associated with this job"
            await self.db.commit()
            return {"status": "error", "error": "No training file found"}
        
        training_file = job.training_file
        model_output_dir = os.path.join("models", "finetuned", f"llama-3-lora-{datetime.now().strftime('%Y%m%d_%H%M%S')}")
        os.makedirs(model_output_dir, exist_ok=True)
        
        # Build the command to run LoRA fine-tuning
        base_model = os.environ.get("BASE_MODEL", "meta-llama/Meta-Llama-3-8B")
        lora_command = [
            "python", "-m", "backend.lora.train",
            "--model_name", base_model,
            "--train_file", training_file,
            "--output_dir", model_output_dir,
            "--batch_size", "4",
            "--micro_batch_size", "1",
            "--num_epochs", "3",
            "--learning_rate", "3e-4",
            "--cutoff_len", "2048",
            "--val_set_size", "0",
            "--lora_r", "8",
            "--lora_alpha", "16",
            "--lora_dropout", "0.05",
            "--target_modules", "q_proj,v_proj,k_proj,o_proj"
        ]
        
        try:
            # In production, we would run this asynchronously
            # For now, we'll log the command but not actually run it
            logger.info(f"Would execute: {' '.join(lora_command)}")
            
            # For testing, we can simulate success
            # In production, we'd actually run the command and wait for completion
            # subprocess.Popen(lora_command)
            
            # Create a model version entry
            version = f"v1-{datetime.now().strftime('%Y%m%d')}"
            model_version = ModelVersion(
                model_name=base_model,
                version=version,
                path=model_output_dir,
                is_active=False
            )
            self.db.add(model_version)
            
            # Update job with model version
            job.model_version_id = model_version.id
            job.completed_at = datetime.now()
            job.status = "completed"
            await self.db.commit()
            
            return {
                "status": "completed",
                "job_id": job.id,
                "model_version": {
                    "id": model_version.id,
                    "model_name": model_version.model_name,
                    "version": model_version.version,
                    "path": model_version.path,
                    "is_active": model_version.is_active
                }
            }
            
        except Exception as e:
            logger.error(f"Error starting training job: {str(e)}", exc_info=True)
            job.status = "failed"
            job.error_message = str(e)
            job.completed_at = datetime.now()
            await self.db.commit()
            
            return {
                "status": "error",
                "error": str(e),
                "job_id": job.id
            }
    
    async def list_training_jobs(self) -> List[Dict[str, Any]]:
        """List all training jobs."""
        result = await self.db.execute(
            select(TrainingJob).order_by(TrainingJob.created_at.desc())
        )
        jobs = result.scalars().all()
        
        return [{
            "id": job.id,
            "status": job.status,
            "feedback_count": job.feedback_count,
            "created_at": job.created_at.isoformat() if job.created_at else None,
            "started_at": job.started_at.isoformat() if job.started_at else None,
            "completed_at": job.completed_at.isoformat() if job.completed_at else None,
            "error_message": job.error_message,
            "model_version_id": job.model_version_id
        } for job in jobs]

async def create_model_version(db: AsyncSession, model_name: str, version: str, path: str) -> Dict[str, Any]:
    """Create a new model version entry."""
    model = ModelVersion(
        model_name=model_name,
        version=version,
        path=path,
        is_active=False
    )
    db.add(model)
    await db.commit()
    await db.refresh(model)
    
    return {
        "id": model.id,
        "model_name": model.model_name,
        "version": model.version,
        "path": model.path,
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
    result = await db.execute(
        select(ModelVersion).where(ModelVersion.id == model_id)
    )
    model = result.scalars().first()
    
    if not model:
        return {"status": "error", "error": "Model not found"}
    
    model.is_active = True
    await db.commit()
    
    # In a production system, here we would reload the LLM with the new model
    # get_llm_service().reload_model(model.path)
    
    return {
        "status": "success",
        "model": {
            "id": model.id,
            "model_name": model.model_name,
            "version": model.version,
            "path": model.path,
            "is_active": model.is_active
        }
    }

async def list_model_versions(db: AsyncSession) -> List[Dict[str, Any]]:
    """List all model versions."""
    result = await db.execute(
        select(ModelVersion).order_by(ModelVersion.created_at.desc())
    )
    models = result.scalars().all()
    
    return [{
        "id": model.id,
        "model_name": model.model_name,
        "version": model.version,
        "path": model.path,
        "created_at": model.created_at.isoformat() if model.created_at else None,
        "is_active": model.is_active
    } for model in models]
