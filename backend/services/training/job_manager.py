
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from typing import Dict, Any, List
import logging
import os
import subprocess
from datetime import datetime

from backend.models.database_models import TrainingJob, ModelVersion

logger = logging.getLogger("training-job-manager")

async def start_training_job(db: AsyncSession, job_id: str) -> Dict[str, Any]:
    """Start a training job."""
    result = await db.execute(
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
    await db.commit()
    
    # Get the training file path
    if not job.training_file:
        job.status = "failed"
        job.error_message = "No training file associated with this job"
        await db.commit()
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
        from backend.services.training.model_version_manager import create_model_version
        
        version = f"v1-{datetime.now().strftime('%Y%m%d')}"
        model_version = ModelVersion(
            model_name=base_model,
            version=version,
            path=model_output_dir,
            is_active=False
        )
        db.add(model_version)
        
        # Update job with model version
        job.model_version_id = model_version.id
        job.completed_at = datetime.now()
        job.status = "completed"
        await db.commit()
        
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
        await db.commit()
        
        return {
            "status": "error",
            "error": str(e),
            "job_id": job.id
        }

async def list_training_jobs(db: AsyncSession) -> List[Dict[str, Any]]:
    """List all training jobs."""
    result = await db.execute(
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
