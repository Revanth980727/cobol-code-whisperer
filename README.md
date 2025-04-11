
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

## Getting Started

### Prerequisites

- Node.js & npm installed ([install with nvm](https://github.com/nvm-sh/nvm#installing-and-updating))
- A machine with GPU support for optimal local LLaMA 3 performance

### Installation

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

## Usage

1. Navigate to the application in your web browser
2. Click on "Upload" in the navigation menu
3. Drag and drop your COBOL source file or click to select a file
4. The system will process the code and generate documentation
5. Review the generated documentation and provide feedback to improve future results

## Project Architecture

The COBOL Code Whisperer consists of:

- **Frontend**: React-based UI with file upload, code viewing, and documentation display
- **COBOL Parser**: Processes and chunks COBOL code for analysis
- **Local LLaMA 3**: Provides AI-powered analysis and documentation generation
- **Feedback System**: Captures user feedback for continuous model improvement
- **RFHL Pipeline**: Uses feedback to fine-tune the LLaMA model for better results

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- The COBOL programming community
- LLaMA 3 and open-source AI research
- All contributors and testers who provide valuable feedback

