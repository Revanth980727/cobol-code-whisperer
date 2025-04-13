
#!/bin/bash
set -e

# Configuration
MODEL_DIR="/app/models"
DEFAULT_MODEL="llama-3-8b-instruct.gguf"
MODEL_PATH=${LLAMA_MODEL_PATH:-"${MODEL_DIR}/${DEFAULT_MODEL}"}

# Create model directory if it doesn't exist
mkdir -p ${MODEL_DIR}

# Check if model exists, download if needed
if [[ ! -f "${MODEL_PATH}" && "${USE_LLAMA_CPP}" == "true" ]]; then
    echo "Model not found at ${MODEL_PATH}. Please make sure to mount a volume with the model file."
    echo "You can download GGUF models from https://huggingface.co/TheBloke/"
    echo "For example: wget -O models/llama-3-8b-instruct.gguf https://huggingface.co/TheBloke/Llama-3-8B-Instruct-GGUF/resolve/main/llama-3-8b-instruct.Q5_K_M.gguf"
    
    # Check if we have a URL to download from
    if [[ ! -z "${MODEL_DOWNLOAD_URL}" ]]; then
        echo "Attempting to download model from ${MODEL_DOWNLOAD_URL}..."
        wget -O "${MODEL_PATH}" "${MODEL_DOWNLOAD_URL}" || {
            echo "Model download failed. Please check the MODEL_DOWNLOAD_URL or manually provide the model file."
            exit 1
        }
    else
        # No download URL provided, check for fallback models
        echo "Checking for fallback models..."
        if ls ${MODEL_DIR}/*.gguf 1> /dev/null 2>&1; then
            FALLBACK_MODEL=$(ls ${MODEL_DIR}/*.gguf | head -n 1)
            echo "Using fallback model: ${FALLBACK_MODEL}"
            export LLAMA_MODEL_PATH="${FALLBACK_MODEL}"
        else
            # If there's a default HF model specified, we can continue with that
            if [[ ! -z "${DEFAULT_HF_MODEL}" ]]; then
                echo "No GGUF model found, but DEFAULT_HF_MODEL is set. Will use HuggingFace model instead."
                export USE_LLAMA_CPP="false"
            else
                echo "No models found. Please provide a model file or download URL."
                exit 1
            fi
        fi
    fi
fi

# Set environment variables for adaptive loading
if [[ "${USE_LLAMA_CPP}" == "true" ]]; then
    echo "Using llama.cpp with model: ${LLAMA_MODEL_PATH}"
    export MODEL_PROVIDER="llama_cpp"
else
    echo "Using HuggingFace Transformers with model: ${DEFAULT_HF_MODEL:-meta-llama/Llama-3-8B-Instruct}"
    export MODEL_PROVIDER="huggingface"
    export HF_MODEL_NAME=${DEFAULT_HF_MODEL:-"meta-llama/Llama-3-8B-Instruct"}
fi

# Install additional Python dependencies if needed
pip install --no-cache-dir -r requirements.txt || echo "Failed to install some dependencies. Continuing..."

# Start the LLM service
echo "Starting LLM Service..."
python -m services.model_providers.llm_service
