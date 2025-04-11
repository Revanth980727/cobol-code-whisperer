import React from 'react';
import { Terminal, Upload, FileCode, Settings, Code2, FileText, Github } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Link } from 'react-router-dom';

const Header = () => {
  return (
    <header className="border-b bg-white dark:bg-card shadow-sm">
      <div className="container flex h-16 items-center justify-between">
        <Link to="/" className="flex items-center gap-2">
          <Terminal className="h-6 w-6 text-cobol-blue" />
          <span className="text-xl font-bold bg-gradient-to-r from-cobol-blue to-cobol-lightBlue bg-clip-text text-transparent">
            COBOL Code Whisperer
          </span>
        </Link>
        
        <nav className="hidden md:flex items-center gap-6">
          <Link to="/" className="text-sm font-medium transition-colors hover:text-primary">
            Home
          </Link>
          <Link to="/upload" className="text-sm font-medium transition-colors hover:text-primary flex items-center gap-1">
            <Upload className="h-4 w-4" /> Upload
          </Link>
          <Link to="/documentation" className="text-sm font-medium transition-colors hover:text-primary flex items-center gap-1">
            <FileText className="h-4 w-4" /> Documentation
          </Link>
          <Link to="/about" className="text-sm font-medium transition-colors hover:text-primary">
            About
          </Link>
        </nav>
        
        <div className="flex items-center gap-2">
          <a 
            href="https://github.com/yourusername/cobol-code-whisperer" 
            target="_blank" 
            rel="noopener noreferrer"
            className="text-sm font-medium text-muted-foreground transition-colors hover:text-primary"
          >
            <Button variant="outline" size="sm" className="hidden md:flex gap-1">
              <Github className="h-4 w-4" />
              <span>GitHub</span>
            </Button>
          </a>
          <Button variant="default" size="sm" className="hidden md:inline-flex gap-1">
            <FileCode className="h-4 w-4" />
            <span>Try Demo</span>
          </Button>
          <Button variant="ghost" size="icon" className="md:hidden">
            <Code2 className="h-5 w-5" />
          </Button>
        </div>
      </div>
    </header>
  );
};

export default Header;
