
#!/bin/bash
set -e

# Create necessary directories
mkdir -p data/files
mkdir -p data/feedback
mkdir -p data/training
mkdir -p models/finetuned

# Show current directory structure for debugging
echo "Current directory: $(pwd)"
echo "Directory contents:"
ls -la

# Run database migrations with proper path
echo "Running database migrations..."
export PYTHONPATH=$(pwd)
alembic upgrade head

# Start the FastAPI application
echo "Starting COBOL Whisperer API..."
exec uvicorn main:app --host ${HOST:-0.0.0.0} --port ${PORT:-8000}
