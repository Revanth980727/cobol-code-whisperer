
#!/bin/bash
set -e

# Run database migrations
echo "Running database migrations..."
alembic upgrade head

# Start the FastAPI application
echo "Starting COBOL Whisperer API..."
exec uvicorn main:app --host ${HOST:-0.0.0.0} --port ${PORT:-8000}
