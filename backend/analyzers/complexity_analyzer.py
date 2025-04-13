
import re
from typing import Dict, Any

def calculate_complexity(code: str) -> Dict[str, Any]:
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
