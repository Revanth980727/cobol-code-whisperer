
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.future import select
from sqlalchemy.orm import selectinload
import logging
from typing import Optional, Dict, List, Any
import json

from backend.models.database_models import UploadedFile, AnalysisResult, CodeChunk, Feedback

logger = logging.getLogger("db-file-service")

async def store_file_db(db: AsyncSession, file_id: str, filename: str, content: bytes, lines: int) -> UploadedFile:
    """Store a file in the database."""
    db_file = UploadedFile(
        id=file_id,
        filename=filename,
        content=content.decode('utf-8', errors='replace'),  # Store as text
        size=len(content),
        lines=lines,
    )
    db.add(db_file)
    await db.commit()
    await db.refresh(db_file)
    return db_file

async def get_file_db(db: AsyncSession, file_id: str) -> Optional[Dict[str, Any]]:
    """Retrieve a file from the database by ID."""
    result = await db.execute(select(UploadedFile).where(UploadedFile.id == file_id))
    file = result.scalars().first()
    
    if file:
        return {
            "filename": file.filename,
            "content": file.content,
            "size": file.size,
            "lines": file.lines,
            "upload_date": file.upload_date.isoformat() if file.upload_date else None,
        }
    return None

async def file_exists_db(db: AsyncSession, file_id: str) -> bool:
    """Check if a file exists in the database."""
    result = await db.execute(
        select(UploadedFile.id).where(UploadedFile.id == file_id)
    )
    return result.scalar() is not None

async def store_analysis_result(
    db: AsyncSession, 
    file_id: str,
    summary: str,
    business_rules: List[str],
    complexity: Dict[str, Any],
    chunks: List[Dict[str, Any]] = None
) -> AnalysisResult:
    """Store analysis results in the database."""
    # Create the analysis result
    analysis = AnalysisResult(
        file_id=file_id,
        summary=summary,
        business_rules=json.dumps(business_rules),
        complexity_score=complexity.get("score", 0),
        lines_of_code=complexity.get("linesOfCode", 0),
        comment_percentage=complexity.get("commentPercentage", 0),
        cyclomatic_complexity=complexity.get("cyclomatic", 0),
    )
    
    db.add(analysis)
    await db.flush()
    
    # Add code chunks if provided
    if chunks:
        for chunk in chunks:
            db_chunk = CodeChunk(
                analysis_id=analysis.id,
                chunk_type=chunk.get("type", "UNKNOWN"),
                name=chunk.get("name", "Unnamed"),
                content=chunk.get("content", ""),
                start_line=chunk.get("start_line", 0),
                end_line=chunk.get("end_line", 0),
                line_count=chunk.get("line_count", 0),
            )
            db.add(db_chunk)
    
    await db.commit()
    await db.refresh(analysis)
    return analysis

async def get_analysis_result(db: AsyncSession, file_id: str) -> Optional[Dict[str, Any]]:
    """Get analysis result for a file."""
    result = await db.execute(
        select(AnalysisResult)
        .where(AnalysisResult.file_id == file_id)
        .options(selectinload(AnalysisResult.code_chunks))
    )
    
    analysis = result.scalars().first()
    if not analysis:
        return None
    
    # Format the result
    chunks = []
    for chunk in analysis.code_chunks:
        chunks.append({
            "id": chunk.id,
            "type": chunk.chunk_type,
            "name": chunk.name,
            "content": chunk.content,
            "start_line": chunk.start_line,
            "end_line": chunk.end_line,
            "line_count": chunk.line_count,
        })
    
    return {
        "file_id": file_id,
        "summary": analysis.summary,
        "business_rules": json.loads(analysis.business_rules),
        "complexity": {
            "cyclomatic": analysis.cyclomatic_complexity,
            "linesOfCode": analysis.lines_of_code,
            "commentPercentage": analysis.comment_percentage,
            "score": analysis.complexity_score,
        },
        "chunks": chunks,
        "analysis_date": analysis.analysis_date.isoformat() if analysis.analysis_date else None,
    }
