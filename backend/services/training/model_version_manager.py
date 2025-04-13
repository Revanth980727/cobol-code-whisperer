
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from typing import Dict, Any, List
import logging
from datetime import datetime

from backend.models.database_models import ModelVersion

logger = logging.getLogger("model-version-manager")

async def create_model_version(db: AsyncSession, model_name: str, version: str, path: str = None) -> Dict[str, Any]:
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
