
import re
import logging
from typing import List, Dict, Any, Optional
from models.cobol_chunk import CobolChunk

# Configure logging
logger = logging.getLogger("cobol_parser")

class ChunkExtractor:
    """Extracts code chunks like sections and paragraphs from COBOL code"""
    
    @staticmethod
    def extract_sections_and_paragraphs(divisions, chunks, lines):
        """Extract sections and paragraphs from the PROCEDURE DIVISION"""
        procedure_div_index = -1
        for i, chunk in enumerate(chunks):
            if chunk.chunk_type == "division" and "PROCEDURE DIVISION" in chunk.name:
                procedure_div_index = i
                break
        
        if procedure_div_index < 0:
            return divisions, chunks
        
        procedure_div = chunks[procedure_div_index]
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
                for div in divisions:
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
            
            chunks.append(CobolChunk(
                "section", 
                section_name, 
                section_content, 
                abs_start_line, 
                abs_end_line
            ))
            
            # Now find paragraphs within this section
            divisions, chunks = ChunkExtractor.extract_paragraphs(
                divisions, chunks, section_content, section_name, 
                abs_start_line, abs_end_line
            )
        
        # If no sections found, extract paragraphs directly from procedure division
        if not section_locations:
            divisions, chunks = ChunkExtractor.extract_paragraphs(
                divisions, chunks, procedure_div.content, None,
                procedure_div.start_line, procedure_div.end_line
            )
        
        return divisions, chunks
    
    @staticmethod
    def extract_paragraphs(divisions, chunks, content, parent_section, start_line, end_line):
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
                for div in divisions:
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
            
            chunks.append(CobolChunk(
                "paragraph", 
                para_name, 
                para_content, 
                para_line, 
                abs_end_line
            ))
        
        return divisions, chunks
