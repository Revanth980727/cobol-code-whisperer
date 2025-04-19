
from jinja2 import Template
from typing import Dict, Any, List, Optional

class PromptTemplateEngine:
    """Template engine for LLM prompts using Jinja2."""
    
    # COBOL Analysis templates
    PROGRAM_SUMMARY_TEMPLATE = """
You are an expert COBOL programmer and analyst. Analyze the following COBOL code and provide:
1. A concise summary of what the program does (main business purpose)
2. The key business rules implemented in the code

COBOL CODE:
```
{{ code }}
```

Provide your analysis in a structured JSON format with 'summary' and 'business_rules' keys.
Make your summary concise (2-3 sentences) and focus on business function, not technical details.
For business_rules, provide an array of strings, each describing one business rule.
"""

    DATA_DIVISION_TEMPLATE = """
As a COBOL expert, analyze this DATA DIVISION:

```cobol
{{ code }}
```

Identify the key data structures, files, and variables used in the program.
List important business data elements and their purposes.

Provide your response in JSON format with 'summary' and 'business_rules' keys.
"""

    PROCEDURE_DIVISION_TEMPLATE = """
As a COBOL expert, analyze this PROCEDURE DIVISION:

```cobol
{{ code }}
```

Summarize the main processing flow and key business operations.
Identify the primary business rules implemented in this code.

Provide your response in JSON format with 'summary' and 'business_rules' keys.
"""

    SECTION_TEMPLATE = """
As a COBOL expert, analyze this {{ section_name }} SECTION:

```cobol
{{ code }}
```

Explain the purpose of this section and its role in the program's business logic.
Identify any business rules implemented here.

Provide your response in JSON format with 'summary' and 'business_rules' keys.
"""

    PARAGRAPH_TEMPLATE = """
As a COBOL expert, analyze this {{ paragraph_name }} paragraph:

```cobol
{{ code }}
```

Explain what this paragraph does in business terms.
Identify any specific business rules implemented in this paragraph.

Provide your response in JSON format with 'summary' and 'business_rules' keys.
"""

    @classmethod
    def render_template(cls, template_string: str, context: Dict[str, Any]) -> str:
        """Render a template with the provided context."""
        template = Template(template_string)
        return template.render(**context)
    
    @classmethod
    def program_summary_prompt(cls, code: str) -> str:
        """Generate a prompt for overall program summary."""
        # Truncate code to 3500 characters to stay within token limits
        return cls.render_template(cls.PROGRAM_SUMMARY_TEMPLATE, {"code": code[:3500]})
    
    @classmethod
    def data_division_prompt(cls, code: str) -> str:
        """Generate a prompt for DATA DIVISION analysis."""
        return cls.render_template(cls.DATA_DIVISION_TEMPLATE, {"code": code[:2500]})
    
    @classmethod
    def procedure_division_prompt(cls, code: str) -> str:
        """Generate a prompt for PROCEDURE DIVISION analysis."""
        return cls.render_template(cls.PROCEDURE_DIVISION_TEMPLATE, {"code": code[:3000]})
    
    @classmethod
    def section_prompt(cls, section_name: str, code: str) -> str:
        """Generate a prompt for section analysis."""
        return cls.render_template(cls.SECTION_TEMPLATE, {
            "section_name": section_name, 
            "code": code[:2500]
        })
    
    @classmethod
    def paragraph_prompt(cls, paragraph_name: str, code: str) -> str:
        """Generate a prompt for paragraph analysis."""
        return cls.render_template(cls.PARAGRAPH_TEMPLATE, {
            "paragraph_name": paragraph_name, 
            "code": code[:2000]
        })
