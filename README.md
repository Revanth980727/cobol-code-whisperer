
# COBOL Code Whisperer

An AI-powered tool for analyzing and documenting legacy COBOL code.

## Overview

COBOL Code Whisperer is a web application that uses LLaMA to analyze COBOL code and generate documentation, business rules, and structural insights. It provides advanced visualizations of code structure and complexity metrics to help developers understand legacy COBOL systems.

## Features

- COBOL code parsing and analysis
- AI-powered documentation generation
- Visual code structure diagrams
- Complexity metrics and analysis
- Business rule extraction
- User feedback collection for continuous improvement
- Database persistence with SQLite and SQLAlchemy
- LoRA fine-tuning capabilities
- Full Docker support with separate services for backend, frontend, and LLM

## Architecture

- **Frontend**: React + Vite + Tailwind CSS
- **Backend**: FastAPI + SQLAlchemy + SQLite
- **AI**: LLaMA 3 with separate containerized service

## Getting Started

### Prerequisites

- Docker and Docker Compose (recommended)
- Alternatively: Python 3.10+ and Node.js 18+
- Optional: NVIDIA GPU with CUDA support for accelerated LLM inference

### Running with Docker (Recommended)

1. Clone the repository:
```bash
git clone https://github.com/yourusername/cobol-whisperer.git
cd cobol-whisperer
```

2. Download the LLaMA 3 model:
```bash
mkdir -p models
# Download your preferred LLaMA 3 model in GGUF format
# Example: wget -O models/llama-3-8b-instruct.gguf https://huggingface.co/TheBloke/Llama-3-8B-Instruct-GGUF/resolve/main/llama-3-8b-instruct.Q5_K_M.gguf
```

3. Start all services with a single command:
```bash
docker-compose up -d
```

4. Access the web interface at http://localhost:5173

### Service Architecture

The application is containerized into three separate services:

1. **Frontend (port 5173)**: React application served through Nginx
2. **Backend (port 8000)**: FastAPI application for code analysis and database operations
3. **LLM Service (port 8080)**: Dedicated service for LLaMA model inference

### Using GPU Acceleration

For GPU acceleration, ensure you have:
- NVIDIA GPU with up-to-date drivers
- [NVIDIA Container Toolkit](https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/install-guide.html) installed

The docker-compose.yml is configured to use GPU if available.

## Configuration Options

The application can be configured using environment variables:

### Backend Service
- `DATABASE_URL`: SQLite connection string
- `USE_LLM_SERVICE`: Set to "true" to use the dedicated LLM service
- `LLM_SERVICE_URL`: URL of the LLM service
- `DEBUG`: Enable debug mode

### LLM Service
- `USE_LLAMA_CPP`: Set to "true" to use llama.cpp (recommended for production)
- `LLAMA_MODEL_PATH`: Path to the LLaMA model file

## Manual Setup

Refer to the backend and frontend README files for instructions on setting up the application without Docker.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.
