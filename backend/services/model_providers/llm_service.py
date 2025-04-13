
"""
Standalone LLM service for Docker deployment
"""

import os
import logging
import json
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import parse_qs, urlparse

# Import model provider
from services.model_providers.llama_cpp_provider import LlamaCppProvider
from services.model_providers.huggingface_provider import HuggingFaceProvider

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("llm-service")

class LLMRequestHandler(BaseHTTPRequestHandler):
    """HTTP request handler for LLM inference requests"""
    
    def __init__(self, *args, **kwargs):
        self.model_provider = None
        super().__init__(*args, **kwargs)
    
    def do_GET(self):
        """Handle GET requests"""
        if self.path == '/health':
            self._handle_health_check()
        elif self.path == '/info':
            self._handle_model_info()
        else:
            self.send_error(404, "Not Found")
    
    def do_POST(self):
        """Handle POST requests"""
        if self.path == '/generate':
            self._handle_generate()
        else:
            self.send_error(404, "Not Found")
    
    def _handle_health_check(self):
        """Handle health check requests"""
        is_ready = self.server.model_provider.is_model_loaded()
        status = {
            "status": "healthy" if is_ready else "loading",
            "model_loaded": is_ready
        }
        
        self._send_json_response(200, status)
    
    def _handle_model_info(self):
        """Handle model info requests"""
        info = self.server.model_provider.get_model_info()
        self._send_json_response(200, info)
    
    def _handle_generate(self):
        """Handle text generation requests"""
        content_length = int(self.headers.get('Content-Length', 0))
        if content_length == 0:
            self.send_error(400, "Empty request")
            return
        
        post_data = self.rfile.read(content_length).decode('utf-8')
        try:
            data = json.loads(post_data)
            prompt = data.get('prompt', '')
            max_tokens = data.get('max_tokens', 2000)
            
            if not prompt:
                self.send_error(400, "Missing prompt parameter")
                return
            
            generated_text = self.server.model_provider.generate_text(prompt, max_tokens)
            response = {"text": generated_text}
            self._send_json_response(200, response)
        except json.JSONDecodeError:
            self.send_error(400, "Invalid JSON")
        except Exception as e:
            logger.error(f"Error generating text: {str(e)}")
            self.send_error(500, f"Error generating text: {str(e)}")
    
    def _send_json_response(self, code, data):
        """Send JSON response"""
        self.send_response(code)
        self.send_header('Content-type', 'application/json')
        self.end_headers()
        self.wfile.write(json.dumps(data).encode('utf-8'))

class LLMServer(HTTPServer):
    """HTTP server with LLM capabilities"""
    
    def __init__(self, server_address, RequestHandlerClass):
        super().__init__(server_address, RequestHandlerClass)
        self._initialize_model_provider()
    
    def _initialize_model_provider(self):
        """Initialize the model provider"""
        use_llama_cpp = os.environ.get("USE_LLAMA_CPP", "false").lower() == "true"
        
        logger.info(f"Initializing model provider (use_llama_cpp={use_llama_cpp})")
        
        if use_llama_cpp:
            self.model_provider = LlamaCppProvider()
        else:
            self.model_provider = HuggingFaceProvider()

def run_server(host="0.0.0.0", port=8080):
    """Run the LLM server"""
    server_address = (host, port)
    httpd = LLMServer(server_address, LLMRequestHandler)
    
    logger.info(f"Starting LLM server on {host}:{port}")
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        logger.info("Shutting down LLM server")
        httpd.server_close()

if __name__ == "__main__":
    host = os.environ.get("HOST", "0.0.0.0")
    port = int(os.environ.get("PORT", 8080))
    run_server(host, port)
