
from fastapi import FastAPI, UploadFile, File, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import Optional, List, Dict, Any, Union
import os
import uuid
import re
import torch
from transformers import AutoModelForCausalLM, AutoTokenizer
from llama_cpp import Llama
import json
import logging
import networkx as nx
from datetime import datetime

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

# Path to LLaMA 3 model and configuration
MODEL_PATH = os.environ.get("LLAMA_MODEL_PATH", "meta-llama/Meta-Llama-3-8B-Instruct")
USE_LLAMA_CPP = os.environ.get("USE_LLAMA_CPP", "false").lower() == "true"
LLAMA_MODEL_LOADED = False
llm = None
tokenizer = None
model = None

# Initialize LLaMA model based on environment
try:
    if USE_LLAMA_CPP:
        # Use llama.cpp for optimized inference
        logger.info(f"Loading LLaMA 3 model using llama.cpp from {MODEL_PATH}")
        llm = Llama(
            model_path=MODEL_PATH,
            n_ctx=4096,  # Context window size
            n_threads=4  # Adjust based on your CPU
        )
        LLAMA_MODEL_LOADED = llm is not None
        logger.info(f"LLaMA 3 model loaded successfully: {LLAMA_MODEL_LOADED}")
    else:
        # Use HuggingFace Transformers
        logger.info(f"Loading LLaMA 3 model using HuggingFace Transformers from {MODEL_PATH}")
        tokenizer = AutoTokenizer.from_pretrained(MODEL_PATH)
        model = AutoModelForCausalLM.from_pretrained(
            MODEL_PATH,
            torch_dtype=torch.float16,
            device_map="auto",
            low_cpu_mem_usage=True
        )
        LLAMA_MODEL_LOADED = (tokenizer is not None and model is not None)
        logger.info(f"LLaMA 3 model loaded successfully: {LLAMA_MODEL_LOADED}")
except Exception as e:
    logger.error(f"Failed to load LLaMA model: {str(e)}")
    LLAMA_MODEL_LOADED = False


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


class CobolChunk:
    """Represents a meaningful chunk of COBOL code"""
    def __init__(self, chunk_type, name, content, start_line, end_line):
        self.chunk_type = chunk_type  # division, section, paragraph
        self.name = name
        self.content = content
        self.start_line = start_line
        self.end_line = end_line
    
    def to_dict(self):
        return {
            "type": self.chunk_type,
            "name": self.name,
            "content": self.content,
            "start_line": self.start_line,
            "end_line": self.end_line,
            "line_count": self.end_line - self.start_line + 1
        }


class CobolParser:
    """Enhanced COBOL parser that extracts structure, chunks, and relationships"""
    
    def __init__(self, code):
        self.code = code
        self.lines = code.split('\n')
        self.chunks = []
        self.divisions = []
        self.call_graph = {}
        self.data_flow = {}
        
    def parse(self):
        """Parse the COBOL code structure"""
        self._extract_divisions()
        self._extract_sections_and_paragraphs()
        self._build_call_graph()
        self._analyze_data_flow()
        return {
            "divisions": self.divisions,
            "chunks": [chunk.to_dict() for chunk in self.chunks],
            "call_graph": self.call_graph,
            "data_flow": self.data_flow
        }
        
    def _extract_divisions(self):
        """Extract COBOL divisions from the code"""
        division_patterns = {
            "IDENTIFICATION DIVISION": r"^\s*IDENTIFICATION\s+DIVISION\s*\.",
            "ENVIRONMENT DIVISION": r"^\s*ENVIRONMENT\s+DIVISION\s*\.",
            "DATA DIVISION": r"^\s*DATA\s+DIVISION\s*\.",
            "PROCEDURE DIVISION": r"^\s*PROCEDURE\s+DIVISION\s*\."
        }
        
        division_locations = {}
        
        # Find the divisions
        for div_name, pattern in division_patterns.items():
            for i, line in enumerate(self.lines):
                if re.search(pattern, line, re.IGNORECASE):
                    division_locations[div_name] = i
                    self.divisions.append({
                        "division": div_name,
                        "line": i + 1,
                        "elements": []
                    })
        
        # Extract content for each division
        sorted_divs = sorted(division_locations.items(), key=lambda x: x[1])
        for i, (div_name, start_line) in enumerate(sorted_divs):
            end_line = len(self.lines) - 1
            if i < len(sorted_divs) - 1:
                end_line = sorted_divs[i+1][1] - 1
            
            division_content = '\n'.join(self.lines[start_line:end_line+1])
            self.chunks.append(CobolChunk(
                "division", 
                div_name, 
                division_content, 
                start_line + 1, 
                end_line + 1
            ))
    
    def _extract_sections_and_paragraphs(self):
        """Extract sections and paragraphs from the PROCEDURE DIVISION"""
        procedure_div_index = -1
        for i, chunk in enumerate(self.chunks):
            if chunk.chunk_type == "division" and "PROCEDURE DIVISION" in chunk.name:
                procedure_div_index = i
                break
        
        if procedure_div_index < 0:
            return
        
        procedure_div = self.chunks[procedure_div_index]
        procedure_lines = procedure_div.content.split('\n')
        
        # Find sections
        section_pattern = r"^\s*([A-Za-z0-9-]+)\s+SECTION\s*\."
        section_locations = {}
        
        for i, line in enumerate(procedure_lines):
            section_match = re.search(section_pattern, line, re.IGNORECASE)
            if section_match:
                section_name = section_match.group(1)
                section_line = procedure_div.start_line + i
                section_locations[section_name] = section_line
                
                # Add section to the appropriate division
                for div in self.divisions:
                    if "PROCEDURE DIVISION" in div["division"]:
                        div["elements"].append({
                            "name": section_name,
                            "description": f"{section_name} section"
                        })
        
        # Extract content for each section
        sorted_sections = sorted(section_locations.items(), key=lambda x: x[1])
        for i, (section_name, section_line) in enumerate(sorted_sections):
            abs_start_line = section_line
            rel_start_line = section_line - procedure_div.start_line
            
            end_rel_line = len(procedure_lines) - 1
            if i < len(sorted_sections) - 1:
                end_rel_line = sorted_sections[i+1][1] - procedure_div.start_line - 1
            
            section_content = '\n'.join(procedure_lines[rel_start_line:end_rel_line+1])
            abs_end_line = procedure_div.start_line + end_rel_line
            
            self.chunks.append(CobolChunk(
                "section", 
                section_name, 
                section_content, 
                abs_start_line, 
                abs_end_line
            ))
            
            # Now find paragraphs within this section
            self._extract_paragraphs(section_content, section_name, abs_start_line, abs_end_line)
        
        # If no sections found, extract paragraphs directly from procedure division
        if not section_locations:
            self._extract_paragraphs(procedure_div.content, None, procedure_div.start_line, procedure_div.end_line)
    
    def _extract_paragraphs(self, content, parent_section, start_line, end_line):
        """Extract paragraphs from a section or procedure division"""
        lines = content.split('\n')
        paragraph_pattern = r"^\s*([A-Za-z0-9-]+)\s*\."
        para_locations = {}
        
        for i, line in enumerate(lines):
            # Skip lines that are section declarations
            if re.search(r"\bSECTION\b", line, re.IGNORECASE):
                continue
                
            para_match = re.search(paragraph_pattern, line, re.IGNORECASE)
            if para_match:
                para_name = para_match.group(1)
                # Skip if this is likely not a paragraph (common keywords)
                if para_name.upper() in ["PROGRAM-ID", "AUTHOR", "DATE-WRITTEN", "SECURITY"]:
                    continue
                    
                para_line = start_line + i
                para_locations[para_name] = para_line
                
                # Add paragraph to the appropriate division
                for div in self.divisions:
                    if "PROCEDURE DIVISION" in div["division"]:
                        div["elements"].append({
                            "name": para_name,
                            "description": f"{para_name} paragraph" + (f" in {parent_section} section" if parent_section else "")
                        })
        
        # Extract content for each paragraph
        sorted_paras = sorted(para_locations.items(), key=lambda x: x[1])
        for i, (para_name, para_line) in enumerate(sorted_paras):
            rel_start_line = para_line - start_line
            
            end_rel_line = len(lines) - 1
            if i < len(sorted_paras) - 1:
                end_rel_line = sorted_paras[i+1][1] - start_line - 1
            
            para_content = '\n'.join(lines[rel_start_line:end_rel_line+1])
            abs_end_line = start_line + end_rel_line
            
            self.chunks.append(CobolChunk(
                "paragraph", 
                para_name, 
                para_content, 
                para_line, 
                abs_end_line
            ))
    
    def _build_call_graph(self):
        """Build a call graph showing relationships between paragraphs"""
        # Map of paragraph/section names to their chunks
        name_to_chunk = {
            chunk.name: chunk for chunk in self.chunks 
            if chunk.chunk_type in ["paragraph", "section"]
        }
        
        # Find all PERFORM statements
        perform_pattern = r"\bPERFORM\s+([A-Za-z0-9-]+)"
        
        for chunk in self.chunks:
            if chunk.chunk_type in ["paragraph", "section"]:
                calls = []
                for match in re.finditer(perform_pattern, chunk.content, re.IGNORECASE):
                    called_name = match.group(1)
                    if called_name in name_to_chunk:
                        calls.append(called_name)
                
                if calls:
                    self.call_graph[chunk.name] = calls
    
    def _analyze_data_flow(self):
        """Analyze data flow between variables in the code"""
        # Find all variables from DATA DIVISION
        variables = set()
        
        for chunk in self.chunks:
            if chunk.chunk_type == "division" and "DATA DIVISION" in chunk.name:
                # Basic pattern to find variable declarations (simplified)
                var_pattern = r"^\s*\d+\s+([A-Za-z0-9-]+)"
                for match in re.finditer(var_pattern, chunk.content, re.MULTILINE):
                    variables.add(match.group(1).upper())
        
        # Track usage of variables in paragraphs
        for chunk in self.chunks:
            if chunk.chunk_type == "paragraph":
                variable_usage = []
                for var in variables:
                    # Check for MOVE statements (writing to variable)
                    move_pattern = rf"\bMOVE\s+[A-Za-z0-9-]+\s+TO\s+{var}\b"
                    if re.search(move_pattern, chunk.content, re.IGNORECASE):
                        variable_usage.append({"variable": var, "operation": "write"})
                    
                    # Check for variable reads (in conditions or calculations)
                    read_pattern = rf"\b{var}\b"
                    if re.search(read_pattern, chunk.content, re.IGNORECASE):
                        # Exclude the MOVE TO cases we already captured
                        move_to_pattern = rf"\bMOVE\s+[A-Za-z0-9-]+\s+TO\s+{var}\b"
                        if not re.search(move_to_pattern, chunk.content, re.IGNORECASE):
                            variable_usage.append({"variable": var, "operation": "read"})
                
                if variable_usage:
                    self.data_flow[chunk.name] = variable_usage


def parse_cobol_structure(code: str):
    """Enhanced COBOL code structure parser"""
    parser = CobolParser(code)
    parsed_data = parser.parse()
    return parsed_data["divisions"]


def chunk_cobol_code(code: str):
    """Chunk COBOL code into meaningful segments"""
    parser = CobolParser(code)
    parsed_data = parser.parse()
    return parsed_data["chunks"]


def calculate_complexity(code: str):
    """Calculate code complexity metrics for COBOL code."""
    lines = code.split('\n')
    total_lines = len(lines)
    
    # Count comment lines (lines starting with *)
    comment_lines = sum(1 for line in lines if line.strip().startswith('*'))
    
    # Calculate comment percentage
    comment_percentage = (comment_lines / total_lines) * 100 if total_lines > 0 else 0
    
    # Enhanced cyclomatic complexity calculation
    if_count = len(re.findall(r'\bIF\b', code, re.IGNORECASE))
    evaluate_count = len(re.findall(r'\bEVALUATE\b', code, re.IGNORECASE))
    perform_until_count = len(re.findall(r'\bPERFORM\b.*\bUNTIL\b', code, re.IGNORECASE))
    perform_varying_count = len(re.findall(r'\bPERFORM\b.*\bVARYING\b', code, re.IGNORECASE))
    when_count = len(re.findall(r'\bWHEN\b', code, re.IGNORECASE))
    
    # Basic cyclomatic complexity = 1 + decision points
    cyclomatic = 1 + if_count + evaluate_count + perform_until_count + perform_varying_count + when_count
    
    # Count variables and paragraphs for additional metrics
    data_items = len(re.findall(r'^\s*\d+\s+[A-Za-z0-9-]+', code, re.MULTILINE))
    paragraphs = len(re.findall(r'^\s*[A-Za-z0-9-]+\.', code, re.MULTILINE))
    
    return {
        "cyclomatic": cyclomatic,
        "linesOfCode": total_lines,
        "commentPercentage": round(comment_percentage, 2),
        "dataItems": data_items,
        "paragraphs": paragraphs,
        "ifStatements": if_count,
        "performStatements": perform_until_count + perform_varying_count
    }


def create_llm_prompt(chunk_type: str, chunk_name: str, chunk_content: str) -> str:
    """Create a targeted prompt based on the type of code chunk"""
    if chunk_type == "division" and "IDENTIFICATION DIVISION" in chunk_name:
        return f"""
As a COBOL expert, analyze this IDENTIFICATION DIVISION:

```cobol
{chunk_content}
```

Provide a summary of the program's purpose, author, and creation date if available.
"""
    elif chunk_type == "division" and "DATA DIVISION" in chunk_name:
        return f"""
As a COBOL expert, analyze this DATA DIVISION:

```cobol
{chunk_content}
```

Identify the key data structures, files, and variables used in the program.
List important business data elements and their purposes.
"""
    elif chunk_type == "division" and "PROCEDURE DIVISION" in chunk_name:
        return f"""
As a COBOL expert, analyze this PROCEDURE DIVISION:

```cobol
{chunk_content}
```

Summarize the main processing flow and key business operations.
Identify the primary business rules implemented in this code.
"""
    elif chunk_type == "section":
        return f"""
As a COBOL expert, analyze this {chunk_name} SECTION:

```cobol
{chunk_content}
```

Explain the purpose of this section and its role in the program's business logic.
Identify any business rules implemented here.
"""
    elif chunk_type == "paragraph":
        return f"""
As a COBOL expert, analyze this {chunk_name} paragraph:

```cobol
{chunk_content}
```

Explain what this paragraph does in business terms.
Identify any specific business rules implemented in this paragraph.
"""
    else:
        return f"""
As a COBOL expert, analyze this code segment:

```cobol
{chunk_content}
```

Explain what this code does in business terms.
Identify any specific business logic or rules implemented here.
"""


def analyze_with_llm(code: str, structured_parsing: bool = True) -> dict:
    """Use LLaMA 3 model to analyze COBOL code with chunking."""
    global LLAMA_MODEL_LOADED, llm, model, tokenizer
    
    if not LLAMA_MODEL_LOADED:
        # Fallback if model is not loaded
        logger.warning("LLaMA model not available. Using fallback analysis.")
        return {
            "summary": "LLaMA model not available. Using fallback analysis.",
            "business_rules": [
                "Automatic analysis not available - model not loaded",
                "Please check server logs for model loading errors"
            ]
        }
    
    try:
        # First get a high-level summary of the entire program
        high_level_prompt = f"""
You are an expert COBOL programmer and analyst. Analyze the following COBOL code and provide:
1. A concise summary of what the program does (main business purpose)
2. The key business rules implemented in the code

COBOL CODE:
```
{code[:3500]}  # Limiting to first 3500 chars to fit in context window
```

Provide your analysis in a structured JSON format with 'summary' and 'business_rules' keys.
Make your summary concise (2-3 sentences) and focus on business function, not technical details.
"""

        # Generate high-level summary
        high_level_result = {}
        
        if USE_LLAMA_CPP and llm:
            # Generate using llama.cpp
            response = llm(
                high_level_prompt, 
                max_tokens=2000,
                temperature=0.1,
                top_p=0.9,
                echo=False
            )
            result_text = response["choices"][0]["text"]
        else:
            # Generate using HuggingFace Transformers
            inputs = tokenizer(high_level_prompt, return_tensors="pt").to(model.device)
            with torch.no_grad():
                outputs = model.generate(
                    **inputs,
                    max_new_tokens=2000,
                    temperature=0.1,
                    top_p=0.9,
                )
            result_text = tokenizer.decode(outputs[0], skip_special_tokens=True)
        
        # Extract JSON from the response
        try:
            # Try to extract JSON from the output
            json_match = re.search(r'```json\s*([\s\S]*?)\s*```', result_text)
            if json_match:
                high_level_result = json.loads(json_match.group(1))
            else:
                # Try to extract content between any code blocks
                json_text = re.search(r'```([\s\S]*?)```', result_text)
                if json_text:
                    try:
                        high_level_result = json.loads(json_text.group(1))
                    except:
                        # If that fails, try to manually extract summary and business rules
                        summary_match = re.search(r'"?summary"?\s*[:\=]\s*["\'](.*?)["\']', result_text, re.IGNORECASE | re.DOTALL)
                        rules_match = re.search(r'"?business_?rules"?\s*[:\=]\s*\[(.*?)\]', result_text, re.IGNORECASE | re.DOTALL)
                        
                        high_level_result = {
                            "summary": summary_match.group(1) if summary_match else "Summary extraction failed",
                            "business_rules": []
                        }
                        
                        if rules_match:
                            rules_text = rules_match.group(1)
                            rules = re.findall(r'"([^"]+)"', rules_text)
                            high_level_result["business_rules"] = rules if rules else ["Rules extraction failed"]
                else:
                    # Last resort fallback
                    high_level_result = {
                        "summary": "Failed to extract structured summary from LLM output.",
                        "business_rules": ["Failed to extract business rules from LLM output."]
                    }
        except Exception as e:
            logger.error(f"Failed to parse LLM output as JSON: {e}")
            high_level_result = {
                "summary": result_text[:500],  # First 500 chars as summary
                "business_rules": ["Failed to structure LLM output properly"]
            }
        
        # If we're not doing structured parsing, return the high-level analysis
        if not structured_parsing:
            return high_level_result
            
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
            prompt = create_llm_prompt(chunk["type"], chunk["name"], chunk["content"])
            
            try:
                if USE_LLAMA_CPP and llm:
                    response = llm(
                        prompt,
                        max_tokens=1000,
                        temperature=0.1,
                        echo=False
                    )
                    analysis = response["choices"][0]["text"]
                else:
                    inputs = tokenizer(prompt, return_tensors="pt").to(model.device)
                    with torch.no_grad():
                        outputs = model.generate(
                            **inputs,
                            max_new_tokens=1000,
                            temperature=0.1,
                        )
                    analysis = tokenizer.decode(outputs[0], skip_special_tokens=True)
                
                chunk_analyses.append({
                    "chunk_type": chunk["type"],
                    "chunk_name": chunk["name"],
                    "analysis": analysis
                })
            except Exception as e:
                logger.error(f"Error analyzing chunk {chunk['name']}: {str(e)}")
        
        # Extract additional business rules from chunk analyses
        additional_rules = []
        for chunk_analysis in chunk_analyses:
            analysis_text = chunk_analysis["analysis"]
            
            # Look for text that mentions business rules
            rule_matches = re.finditer(r'(?:business rule|rule|logic)[\s:]+([^\.]+)', analysis_text, re.IGNORECASE)
            for match in rule_matches:
                rule_text = match.group(1).strip()
                if len(rule_text) > 10 and rule_text not in additional_rules:
                    additional_rules.append(rule_text)
        
        # Add additional business rules to the high-level analysis
        existing_rules = high_level_result.get("business_rules", [])
        for rule in additional_rules:
            if rule not in existing_rules:
                existing_rules.append(rule)
        
        high_level_result["business_rules"] = existing_rules
        
        return high_level_result
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
    global LLAMA_MODEL_LOADED
    return {"status": "ready" if LLAMA_MODEL_LOADED else "not_loaded"}

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
    
    # Analyze with LLaMA 3
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
