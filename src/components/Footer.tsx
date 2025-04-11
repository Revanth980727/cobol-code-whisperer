
import React from 'react';
import { Terminal } from 'lucide-react';
import { Link } from 'react-router-dom';

const Footer = () => {
  return (
    <footer className="border-t bg-muted/40">
      <div className="container flex flex-col gap-6 py-8 md:flex-row md:items-center md:justify-between md:py-6">
        <div className="flex flex-col gap-2">
          <div className="flex items-center gap-2">
            <Terminal className="h-5 w-5 text-cobol-blue" />
            <span className="font-medium text-cobol-blue">COBOL Code Whisperer</span>
          </div>
          <p className="text-xs text-muted-foreground">
            AI-powered documentation for legacy COBOL systems.
          </p>
        </div>
        
        <nav className="grid grid-cols-2 gap-8 md:grid-cols-4 md:gap-4">
          <div className="flex flex-col gap-2">
            <h3 className="text-sm font-medium">Product</h3>
            <Link to="/upload" className="text-xs text-muted-foreground hover:text-primary">
              Upload Code
            </Link>
            <Link to="/documentation" className="text-xs text-muted-foreground hover:text-primary">
              Documentation
            </Link>
          </div>
          <div className="flex flex-col gap-2">
            <h3 className="text-sm font-medium">Resources</h3>
            <Link to="/about" className="text-xs text-muted-foreground hover:text-primary">
              About
            </Link>
            <Link to="/faq" className="text-xs text-muted-foreground hover:text-primary">
              FAQ
            </Link>
          </div>
          <div className="flex flex-col gap-2">
            <h3 className="text-sm font-medium">Legal</h3>
            <Link to="/privacy" className="text-xs text-muted-foreground hover:text-primary">
              Privacy
            </Link>
            <Link to="/terms" className="text-xs text-muted-foreground hover:text-primary">
              Terms
            </Link>
          </div>
          <div className="flex flex-col gap-2">
            <h3 className="text-sm font-medium">Connect</h3>
            <a 
              href="https://github.com/yourusername/cobol-code-whisperer" 
              target="_blank" 
              rel="noopener noreferrer"
              className="text-xs text-muted-foreground hover:text-primary"
            >
              GitHub
            </a>
          </div>
        </nav>
      </div>
      <div className="border-t bg-muted/40 py-4">
        <div className="container flex flex-col items-center justify-between gap-1 md:flex-row">
          <p className="text-xs text-muted-foreground">
            © {new Date().getFullYear()} COBOL Code Whisperer. All rights reserved.
          </p>
          <p className="text-xs text-muted-foreground">
            Powered by LLaMA 3 with RFHL (Reinforcement Fine-tuning from Human Labels)
          </p>
        </div>
      </div>
    </footer>
  );
};

export default Footer;
