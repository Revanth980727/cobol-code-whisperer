
from fastapi import FastAPI, UploadFile, File, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import Optional, List, Dict, Any
import os
import uuid
import re
import torch
from transformers import AutoModelForCausalLM, AutoTokenizer
from llama_cpp import Llama

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

# Path to LLaMA 3 model
# Note: In production, you'd download and store the model locally
MODEL_PATH = os.environ.get("LLAMA_MODEL_PATH", "meta-llama/Meta-Llama-3-8B-Instruct")
USE_LLAMA_CPP = os.environ.get("USE_LLAMA_CPP", "false").lower() == "true"

# Initialize LLaMA model based on environment
if USE_LLAMA_CPP:
    # Use llama.cpp for optimized inference
    try:
        llm = Llama(
            model_path=MODEL_PATH,
            n_ctx=4096,  # Context window size
            n_threads=4  # Adjust based on your CPU
        )
        print("Loaded LLaMA 3 model using llama.cpp")
    except Exception as e:
        print(f"Failed to load LLaMA model: {e}")
        llm = None
else:
    # Use HuggingFace Transformers
    try:
        tokenizer = AutoTokenizer.from_pretrained(MODEL_PATH)
        model = AutoModelForCausalLM.from_pretrained(
            MODEL_PATH,
            torch_dtype=torch.float16,
            device_map="auto",
            low_cpu_mem_usage=True
        )
        print("Loaded LLaMA 3 model using HuggingFace Transformers")
    except Exception as e:
        print(f"Failed to load LLaMA model: {e}")
        tokenizer = None
        model = None


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

def parse_cobol_structure(code: str):
    """Parse COBOL code structure to identify divisions, sections, etc."""
    divisions = []
    # Simple regex patterns to detect COBOL divisions
    division_patterns = {
        "IDENTIFICATION DIVISION": r"IDENTIFICATION\s+DIVISION",
        "ENVIRONMENT DIVISION": r"ENVIRONMENT\s+DIVISION",
        "DATA DIVISION": r"DATA\s+DIVISION",
        "PROCEDURE DIVISION": r"PROCEDURE\s+DIVISION"
    }
    
    for name, pattern in division_patterns.items():
        matches = re.finditer(pattern, code, re.IGNORECASE)
        for match in matches:
            start_pos = match.start()
            # Extract a context window after the division declaration
            context = code[start_pos:start_pos+500]
            elements = []
            
            # Look for program ID in IDENTIFICATION DIVISION
            if "IDENTIFICATION DIVISION" in name:
                program_id_match = re.search(r"PROGRAM-ID\.\s+([A-Za-z0-9-]+)", context, re.IGNORECASE)
                if program_id_match:
                    elements.append({
                        "name": "PROGRAM-ID",
                        "description": program_id_match.group(1)
                    })
            
            # Look for sections in PROCEDURE DIVISION
            if "PROCEDURE DIVISION" in name:
                section_matches = re.finditer(r"([A-Za-z0-9-]+)\s+SECTION", context, re.IGNORECASE)
                for section in section_matches:
                    elements.append({
                        "name": section.group(1),
                        "description": f"{section.group(1)} section"
                    })
                
                # Look for paragraphs
                paragraph_matches = re.finditer(r"^([A-Za-z0-9-]+)\.", context, re.MULTILINE)
                for para in paragraph_matches:
                    if para.group(1) not in ["PROGRAM-ID", "AUTHOR", "DATE-WRITTEN"]:
                        elements.append({
                            "name": para.group(1),
                            "description": f"{para.group(1)} paragraph"
                        })
            
            divisions.append({
                "division": name,
                "elements": elements
            })
    
    return divisions

def calculate_complexity(code: str):
    """Calculate code complexity metrics for COBOL code."""
    lines = code.split('\n')
    total_lines = len(lines)
    
    # Count comment lines (lines starting with *)
    comment_lines = sum(1 for line in lines if line.strip().startswith('*'))
    
    # Calculate comment percentage
    comment_percentage = (comment_lines / total_lines) * 100 if total_lines > 0 else 0
    
    # Simple cyclomatic complexity calculation based on IF, EVALUATE, PERFORM UNTIL statements
    if_count = len(re.findall(r'\bIF\b', code, re.IGNORECASE))
    evaluate_count = len(re.findall(r'\bEVALUATE\b', code, re.IGNORECASE))
    perform_until_count = len(re.findall(r'\bPERFORM\b.*\bUNTIL\b', code, re.IGNORECASE))
    
    # Basic cyclomatic complexity = 1 + decision points
    cyclomatic = 1 + if_count + evaluate_count + perform_until_count
    
    return {
        "cyclomatic": cyclomatic,
        "linesOfCode": total_lines,
        "commentPercentage": round(comment_percentage, 2)
    }

def analyze_with_llm(code: str) -> dict:
    """Use LLaMA 3 model to analyze COBOL code."""
    if not (model and tokenizer) and not llm:
        # Fallback if model is not loaded
        return {
            "summary": "LLaMA model not available. Using fallback analysis.",
            "business_rules": [
                "Automatic analysis not available - model not loaded",
                "Please check server logs for model loading errors"
            ]
        }
    
    # Format the prompt for LLaMA 3
    prompt = f"""
You are an expert COBOL programmer and analyst. Analyze the following COBOL code and provide:
1. A summary of what the program does
2. The business rules implemented in the code

COBOL CODE:
```
{code[:3000]}  # Limiting to first 3000 chars to fit in context window
```

Provide your analysis in a structured JSON format with 'summary' and 'business_rules' keys.
    """
    
    try:
        if USE_LLAMA_CPP and llm:
            # Generate using llama.cpp
            response = llm(
                prompt, 
                max_tokens=2000,
                temperature=0.1,
                top_p=0.9,
                echo=False
            )
            result_text = response["choices"][0]["text"]
        else:
            # Generate using HuggingFace Transformers
            inputs = tokenizer(prompt, return_tensors="pt").to(model.device)
            with torch.no_grad():
                outputs = model.generate(
                    **inputs,
                    max_new_tokens=2000,
                    temperature=0.1,
                    top_p=0.9,
                )
            result_text = tokenizer.decode(outputs[0], skip_special_tokens=True)
        
        # Extract JSON from the response - looking for content between ```json and ``` or just parse the text
        try:
            import json
            json_match = re.search(r'```json\s*([\s\S]*?)\s*```', result_text)
            if json_match:
                analysis = json.loads(json_match.group(1))
            else:
                # Try to extract a reasonable structure
                summary_match = re.search(r'summary["\s:]+([^"]+)', result_text, re.IGNORECASE)
                rules_match = re.findall(r'business.?rules["\s:]+\[([\s\S]+?)\]', result_text, re.IGNORECASE)
                
                analysis = {
                    "summary": summary_match.group(1) if summary_match else "Summary extraction failed",
                    "business_rules": []
                }
                
                if rules_match:
                    # Try to parse the rules
                    rules_text = rules_match[0]
                    rules = re.findall(r'"([^"]+)"', rules_text)
                    analysis["business_rules"] = rules if rules else ["Rules extraction failed"]
        except Exception as e:
            print(f"Failed to parse LLM output as JSON: {e}")
            # Fallback - just use the raw text
            analysis = {
                "summary": result_text[:500],  # First 500 chars as summary
                "business_rules": ["Failed to structure LLM output properly"]
            }
            
        return analysis
    except Exception as e:
        print(f"LLM analysis failed: {e}")
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
    if USE_LLAMA_CPP:
        return {"status": "ready" if llm is not None else "not_loaded"}
    else:
        return {"status": "ready" if model is not None and tokenizer is not None else "not_loaded"}

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
        "lines": len(code.splitlines())
    }
    
    # Parse code structure
    code_structure = parse_cobol_structure(code)
    
    # Calculate complexity metrics
    complexity = calculate_complexity(code)
    
    # Analyze with LLaMA 3
    llm_analysis = analyze_with_llm(code)
    
    # Combine all analysis results
    analysis_result = {
        "file_id": file_id,
        "summary": llm_analysis.get("summary", "Analysis not available"),
        "business_rules": llm_analysis.get("business_rules", []),
        "code_structure": code_structure,
        "complexity": complexity
    }
    
    return analysis_result

@app.post("/feedback/")
async def submit_feedback(feedback: FeedbackModel):
    if feedback.file_id not in uploaded_files:
        raise HTTPException(status_code=404, detail="File not found")
    
    # Store the feedback
    feedback_data.append(feedback.dict())
    
    # Here you would trigger or queue the model fine-tuning process
    # based on accumulated feedback
    # In a real implementation, this would be part of a training pipeline
    
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
