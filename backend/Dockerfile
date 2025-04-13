
FROM python:3.10-slim

WORKDIR /app

# Install system dependencies for llama-cpp-python and PyGraphviz
RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    graphviz \
    libgraphviz-dev \
    pkg-config \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Copy requirements file
COPY requirements.txt .

# Install Python dependencies
RUN pip install --no-cache-dir -r requirements.txt

# Copy the application code
COPY . .

# Create necessary directories
RUN mkdir -p data/files data/feedback data/training

# Set environment variables
ENV PYTHONPATH=/app
ENV PORT=8000
ENV HOST=0.0.0.0

# Optional environment variables with defaults
ENV DATABASE_URL=sqlite+aiosqlite:///./cobol_whisperer.db
ENV USE_LLAMA_CPP=false
# ENV LLAMA_MODEL_PATH=/app/models/llama-3-8b-instruct.gguf

# Expose the application port
EXPOSE 8000

# Run the application with a startup script
COPY docker-entrypoint.sh /app/docker-entrypoint.sh
RUN chmod +x /app/docker-entrypoint.sh

ENTRYPOINT ["/app/docker-entrypoint.sh"]
