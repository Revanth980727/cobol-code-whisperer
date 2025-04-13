
#!/bin/bash
set -e

# Create necessary directories
mkdir -p data/files
mkdir -p data/feedback
mkdir -p data/training
mkdir -p models/finetuned

# Run database migrations
echo "Running database migrations..."
alembic upgrade head

# Start the FastAPI application
echo "Starting COBOL Whisperer API..."
exec uvicorn main:app --host ${HOST:-0.0.0.0} --port ${PORT:-8000}
