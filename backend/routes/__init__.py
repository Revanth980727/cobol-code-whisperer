
# Make the routes directory a Python package
from routes.api import router as api_router
from routes.api_file_routes import router as file_router
from routes.api_feedback_routes import router as feedback_router
from routes.api_model_routes import router as model_router
from routes.api_training_routes import router as training_router

# Export all routers
__all__ = [
    "api_router",
    "file_router",
    "feedback_router",
    "model_router",
    "training_router"
]
