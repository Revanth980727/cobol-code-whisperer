
from fastapi import FastAPI, UploadFile, File, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import Optional, List, Dict, Any, Union
import os
import uuid
import logging
from datetime import datetime

# Import our new modules
from cobol_parser import CobolParser, parse_cobol_structure, chunk_cobol_code, calculate_complexity
from llm_service import get_llm_service
from prompt_templates import PromptTemplateEngine

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[logging.StreamHandler()]
)
logger = logging.getLogger("cobol-whisperer")

app = FastAPI(title="COBOL Code Whisperer Backend")

# Configure CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:5173", "http://localhost:8080"],  # Frontend URLs
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# In-memory storage for uploaded files and feedback
# In production, this would use a database
uploaded_files = {}
feedback_data = []

# Initialize LLM service
llm_service = get_llm_service()


class FeedbackModel(BaseModel):
    file_id: str
    rating: int
    comment: Optional[str] = None
    corrected_summary: Optional[str] = None

class AnalysisResponse(BaseModel):
    file_id: str
    summary: str
    business_rules: List[str]
    code_structure: List[dict]
    complexity: dict
    call_graph: Optional[Dict[str, List[str]]] = None
    data_flow: Optional[Dict[str, List[Dict[str, str]]]] = None
    chunks: Optional[List[Dict[str, Any]]] = None


def analyze_with_llm(code: str, structured_parsing: bool = True) -> dict:
    """Use LLM to analyze COBOL code with chunking."""
    
    if not llm_service.is_ready():
        # Fallback if model is not loaded
        logger.warning("LLM not available. Using fallback analysis.")
        return {
            "summary": "LLM not available. Using fallback analysis.",
            "business_rules": [
                "Automatic analysis not available - model not loaded",
                "Please check server logs for model loading errors"
            ]
        }
    
    try:
        # First get a high-level summary of the entire program
        high_level_prompt = PromptTemplateEngine.program_summary_prompt(code)
        
        # Generate high-level summary
        high_level_analysis = llm_service.analyze_code(code)
        
        # If we're not doing structured parsing, return the high-level analysis
        if not structured_parsing:
            return high_level_analysis
            
        # Otherwise, proceed with detailed chunk analysis
        # Parse the code into chunks for detailed analysis
        parser = CobolParser(code)
        parsed_data = parser.parse()
        chunks = parsed_data["chunks"]
        
        # Select important chunks to analyze (to avoid too many API calls)
        # For now, let's analyze all divisions and a few key paragraphs/sections
        chunks_to_analyze = [chunk for chunk in chunks if chunk["type"] == "division"]
        
        # Add some important paragraphs/sections if they exist
        procedure_chunks = [
            chunk for chunk in chunks 
            if chunk["type"] in ["section", "paragraph"] and 
            chunk["line_count"] > 5  # Only substantial paragraphs
        ]
        
        # Sort by length and take top 3
        procedure_chunks.sort(key=lambda x: x["line_count"], reverse=True)
        chunks_to_analyze.extend(procedure_chunks[:3])
        
        # Analyze each selected chunk
        chunk_analyses = []
        for chunk in chunks_to_analyze:
            analysis = llm_service.analyze_code(
                chunk["content"], 
                chunk_type=chunk["type"], 
                chunk_name=chunk["name"]
            )
            
            chunk_analyses.append({
                "chunk_type": chunk["type"],
                "chunk_name": chunk["name"],
                "analysis": analysis
            })
        
        # Extract additional business rules from chunk analyses
        additional_rules = []
        for chunk_analysis in chunk_analyses:
            analysis = chunk_analysis["analysis"]
            
            # Look for business rules in the analysis
            if "business_rules" in analysis and isinstance(analysis["business_rules"], list):
                for rule in analysis["business_rules"]:
                    if rule not in additional_rules and rule not in high_level_analysis.get("business_rules", []):
                        additional_rules.append(rule)
        
        # Add additional business rules to the high-level analysis
        existing_rules = high_level_analysis.get("business_rules", [])
        existing_rules.extend(additional_rules)
        
        high_level_analysis["business_rules"] = existing_rules
        
        return high_level_analysis
    except Exception as e:
        logger.error(f"LLM analysis failed: {e}")
        return {
            "summary": f"LLM analysis failed: {str(e)}",
            "business_rules": ["Error occurred during analysis"]
        }


@app.get("/")
def read_root():
    return {"status": "COBOL Code Whisperer API is running"}

@app.get("/model-status")
def model_status():
    """Check if the LLM model is loaded and ready."""
    return {"status": "ready" if llm_service.is_ready() else "not_loaded"}

@app.post("/analyze-code/")
async def analyze_code(file: UploadFile = File(...)):
    # Save the uploaded file content
    file_id = str(uuid.uuid4())
    file_content = await file.read()
    
    # Decode the content
    try:
        code = file_content.decode('utf-8')
    except UnicodeDecodeError:
        # Try another encoding if utf-8 fails
        try:
            code = file_content.decode('latin1')
        except:
            raise HTTPException(status_code=400, detail="Could not decode file content")
    
    # Store the file content
    uploaded_files[file_id] = {
        "filename": file.filename,
        "content": file_content,
        "size": len(file_content),
        "lines": len(code.splitlines()),
        "timestamp": datetime.now().isoformat()
    }
    
    # Parse code into chunks for analysis
    parser = CobolParser(code)
    parsed_data = parser.parse()
    
    # Calculate complexity metrics
    complexity = calculate_complexity(code)
    
    # Analyze with LLM
    llm_analysis = analyze_with_llm(code)
    
    # Combine all analysis results
    analysis_result = {
        "file_id": file_id,
        "summary": llm_analysis.get("summary", "Analysis not available"),
        "business_rules": llm_analysis.get("business_rules", []),
        "code_structure": parsed_data["divisions"],
        "complexity": complexity,
        "call_graph": parsed_data["call_graph"],
        "data_flow": parsed_data["data_flow"],
        "chunks": [chunk for chunk in parsed_data["chunks"] if chunk["type"] != "division"][:10]  # Return non-division chunks, limited to 10
    }
    
    return analysis_result

@app.post("/feedback/")
async def submit_feedback(feedback: FeedbackModel):
    if feedback.file_id not in uploaded_files:
        raise HTTPException(status_code=404, detail="File not found")
    
    # Store the feedback with timestamp
    feedback_dict = feedback.dict()
    feedback_dict["timestamp"] = datetime.now().isoformat()
    feedback_data.append(feedback_dict)
    
    return {"status": "Feedback received", "feedback_id": len(feedback_data)}

@app.get("/file/{file_id}")
async def get_file_content(file_id: str):
    if file_id not in uploaded_files:
        raise HTTPException(status_code=404, detail="File not found")
    
    return {
        "filename": uploaded_files[file_id]["filename"],
        "content": uploaded_files[file_id]["content"].decode('utf-8'),
        "lines": uploaded_files[file_id]["lines"]
    }

if __name__ == "__main__":
    import uvicorn
    uvicorn.run("main:app", host="0.0.0.0", port=8000, reload=True)
