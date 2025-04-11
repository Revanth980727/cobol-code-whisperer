
# COBOL Code Whisperer

COBOL Code Whisperer is an AI-powered tool designed to analyze legacy COBOL code and generate clear, comprehensive documentation. By leveraging local LLaMA 3 with RFHL (Reinforcement Fine-tuning from Human Labels), this tool helps organizations understand and maintain their legacy systems more effectively.

![COBOL Code Whisperer](https://lovable.dev/opengraph-image-p98pqg.png)

## Features

- **COBOL Code Analysis**: Upload and analyze COBOL source code to extract business logic and documentation
- **AI-Powered Documentation**: Generate human-readable documentation from complex COBOL programs
- **Local Processing**: Uses local LLaMA 3 model for privacy and security
- **Interactive UI**: Modern, user-friendly interface for code viewing and documentation
- **Feedback Loop**: Built-in feedback system to continuously improve AI outputs
- **Continuous Learning**: RFHL (Reinforcement Fine-tuning from Human Labels) ensures improved documentation quality over time

## Project Architecture

This project consists of two main components:

### Frontend (React)
- Modern React application with TypeScript
- UI components built with Tailwind CSS and shadcn/ui
- File upload, code viewing, and documentation display

### Backend (Python)
- FastAPI server for API endpoints
- COBOL parser for code analysis
- Local LLaMA 3 model integration for AI processing
- Feedback collection and model fine-tuning

## Getting Started

### Prerequisites

- Node.js & npm installed ([install with nvm](https://github.com/nvm-sh/nvm#installing-and-updating))
- Python 3.8+ installed
- A machine with GPU support for optimal local LLaMA 3 performance

### Installation

#### Frontend Setup
```sh
# Clone this repository
git clone https://github.com/yourusername/cobol-code-whisperer.git

# Navigate to the project directory
cd cobol-code-whisperer

# Install dependencies
npm install

# Start the development server
npm run dev
```

#### Backend Setup
```sh
# Navigate to the backend directory
cd backend

# Create a virtual environment
python -m venv venv

# Activate the virtual environment
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt

# Start the FastAPI server
uvicorn main:app --reload
```

## Usage

1. Make sure both frontend and backend servers are running
2. Navigate to the application in your web browser (usually http://localhost:5173)
3. Click on "Upload" in the navigation menu
4. Drag and drop your COBOL source file or click to select a file
5. The system will process the code and generate documentation
6. Review the generated documentation and provide feedback to improve future results

## API Endpoints

The backend provides the following API endpoints:

- `GET /`: Health check
- `POST /analyze-code/`: Upload and analyze a COBOL file
- `POST /feedback/`: Submit feedback on analysis results
- `GET /file/{file_id}`: Retrieve content of a previously uploaded file

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- The COBOL programming community
- LLaMA 3 and open-source AI research
- All contributors and testers who provide valuable feedback
