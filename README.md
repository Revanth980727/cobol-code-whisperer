
# COBOL Code Whisperer

An AI-powered tool for analyzing and documenting legacy COBOL code.

## Overview

COBOL Code Whisperer is a web application that uses LLaMA to analyze COBOL code and generate documentation, business rules, and structural insights. It provides advanced visualizations of code structure and complexity metrics to help developers understand legacy COBOL systems.

## Features

- COBOL code parsing and analysis
- AI-powered documentation generation
- Visual code structure diagrams
- Complexity metrics and analysis
- Business rule extraction
- User feedback collection for continuous improvement
- Database persistence with SQLite and SQLAlchemy
- LoRA fine-tuning capabilities
- Full Docker support

## Architecture

- **Frontend**: React + Vite + Tailwind CSS
- **Backend**: FastAPI + SQLAlchemy + SQLite
- **AI**: LLaMA 3 with LoRA fine-tuning

## Getting Started

### Prerequisites

- Docker and Docker Compose (recommended)
- Alternatively: Python 3.10+ and Node.js 18+

### Running with Docker (Recommended)

1. Clone the repository:
```bash
git clone https://github.com/yourusername/cobol-whisperer.git
cd cobol-whisperer
```

2. Start the application:
```bash
docker-compose up
```

3. Access the web interface at http://localhost:5173

### Manual Setup

#### Backend

1. Set up the backend:
```bash
cd backend
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
pip install -r requirements.txt
```

2. Run migrations:
```bash
alembic upgrade head
```

3. Start the backend server:
```bash
uvicorn main:app --reload
```

#### Frontend

1. Install dependencies:
```bash
npm install
```

2. Start the development server:
```bash
npm run dev
```

3. Access the web interface at http://localhost:5173

## Usage

1. Upload a COBOL file through the web interface
2. The system will analyze the code and display:
   - A summary of the code's purpose
   - Extracted business rules
   - Code structure visualization
   - Complexity metrics
   - Detailed code sections with explanations
3. You can provide feedback on the analysis to improve future results

## Fine-tuning the Model

The application includes a LoRA-based fine-tuning pipeline:

1. Collect user feedback through the application
2. Prepare training data with the API endpoint
3. Execute the fine-tuning script
4. Load the fine-tuned model for improved analysis

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.
