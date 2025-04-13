
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from typing import Dict, Any, Optional, List
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
            # Process training data in multiple formats
            training_data = []     # Standard format
            jsonl_records = []     # JSONL format for some frameworks
            hf_dataset_records = [] # HuggingFace dataset format
            
            for feedback in feedback_list:
                # Convert feedback to training example
                training_example = await self._convert_feedback_to_training_example(feedback)
                if training_example:
                    training_data.append(training_example)
                    
                    # Also create JSONL format
                    jsonl_record = {
                        "text": f"Instruction: {training_example['prompt']}\n\nResponse: {training_example['completion']}"
                    }
                    jsonl_records.append(jsonl_record)
                    
                    # And HuggingFace dataset format
                    hf_record = {
                        "instruction": training_example["prompt"],
                        "input": "",  # Empty for non-contextual instructions
                        "output": training_example["completion"],
                        "feedback_id": str(feedback.id)
                    }
                    hf_dataset_records.append(hf_record)
                    
                    # Mark as processed
                    feedback.is_processed_for_training = True
            
            # Create validation split (20% of data)
            train_val_split_index = int(len(training_data) * 0.8)
            training_set = training_data[:train_val_split_index]
            validation_set = training_data[train_val_split_index:]
            
            # Save data in multiple formats
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            outputs = {}
            
            if training_data:
                # Standard JSON format
                std_path = os.path.join(self.training_dir, f"training_data_{timestamp}.json")
                with open(std_path, "w") as f:
                    json.dump(training_data, f, indent=2)
                outputs["standard_json"] = std_path
                
                # JSONL format
                jsonl_path = os.path.join(self.training_dir, f"training_data_{timestamp}.jsonl")
                with open(jsonl_path, "w") as f:
                    for record in jsonl_records:
                        f.write(json.dumps(record) + "\n")
                outputs["jsonl"] = jsonl_path
                
                # HuggingFace format
                hf_path = os.path.join(self.training_dir, f"training_data_{timestamp}_hf.json")
                with open(hf_path, "w") as f:
                    json.dump({"data": hf_dataset_records}, f, indent=2)
                outputs["huggingface"] = hf_path
                
                # Validation set
                if validation_set:
                    val_path = os.path.join(self.training_dir, f"validation_data_{timestamp}.json")
                    with open(val_path, "w") as f:
                        json.dump(validation_set, f, indent=2)
                    outputs["validation"] = val_path
                
                job.status = "ready"
                job.training_file = std_path  # Use standard format as default
                logger.info(f"Training data prepared: {len(training_data)} examples saved in multiple formats")
            else:
                job.status = "no_data"
                logger.info("No valid training examples could be created")
            
            await self.db.commit()
            
            return {
                "status": job.status,
                "job_id": job.id,
                "count": len(training_data),
                "train_count": len(training_set),
                "validation_count": len(validation_set),
                "file_paths": outputs
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
        
    async def generate_holdout_validation_set(self, db: AsyncSession, count: int = 20) -> Dict[str, Any]:
        """Generate a holdout validation set from processed feedback for quality testing"""
        # Get high-quality processed feedback (positive ratings)
        result = await db.execute(
            select(Feedback)
            .where(Feedback.is_processed_for_training == True)
            .where(Feedback.rating > 0)
            .where(Feedback.corrected_summary != None)
            .limit(count)
        )
        feedback_list = result.scalars().all()
        
        if not feedback_list:
            return {"status": "error", "message": "No suitable feedback found for validation"}
            
        validation_examples = []
        for feedback in feedback_list:
            example = await self._convert_feedback_to_training_example(feedback)
            if example:
                validation_examples.append(example)
                
        if not validation_examples:
            return {"status": "error", "message": "Could not create validation examples"}
            
        # Save validation set
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        validation_path = os.path.join(self.training_dir, f"holdout_validation_{timestamp}.json")
        with open(validation_path, "w") as f:
            json.dump(validation_examples, f, indent=2)
            
        return {
            "status": "success",
            "count": len(validation_examples),
            "file_path": validation_path
        }
