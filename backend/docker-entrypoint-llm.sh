
#!/bin/bash
set -e

# Start the LLM service
echo "Starting LLM Service..."
python -m services.model_providers.llm_service
