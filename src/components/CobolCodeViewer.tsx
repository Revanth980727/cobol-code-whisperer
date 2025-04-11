
import React, { useEffect, useRef } from 'react';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { ScrollArea } from '@/components/ui/scroll-area';

interface CobolCodeViewerProps {
  code: string;
  highlightLines?: number[];
}

const CobolCodeViewer: React.FC<CobolCodeViewerProps> = ({ code, highlightLines = [] }) => {
  const codeRef = useRef<HTMLPreElement>(null);

  useEffect(() => {
    if (!codeRef.current) return;
    
    // Basic COBOL syntax highlighting
    let highlightedCode = code;
    
    // Division highlighting
    highlightedCode = highlightedCode.replace(
      /(IDENTIFICATION DIVISION|DATA DIVISION|PROCEDURE DIVISION|ENVIRONMENT DIVISION)\./g, 
      '<span class="cobol-division">$1.</span>'
    );
    
    // Section highlighting
    highlightedCode = highlightedCode.replace(
      /(FILE SECTION|WORKING-STORAGE SECTION|LINKAGE SECTION|LOCAL-STORAGE SECTION|CONFIGURATION SECTION)\./g,
      '<span class="cobol-section">$1.</span>'
    );
    
    // Keywords
    const keywords = [
      'PROGRAM-ID', 'AUTHOR', 'DATE-WRITTEN', 'PIC', 'PICTURE', 'COMP', 'COMPUTATIONAL',
      'PERFORM', 'UNTIL', 'VARYING', 'THRU', 'THROUGH', 'MOVE', 'TO', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE',
      'IF', 'ELSE', 'END-IF', 'EVALUATE', 'WHEN', 'END-EVALUATE', 'GO', 'GO TO',
      'OPEN', 'CLOSE', 'READ', 'WRITE', 'REWRITE', 'DELETE', 'START', 'INVALID KEY',
      'CALL', 'USING', 'BY', 'REFERENCE', 'CONTENT', 'VALUE', 'DISPLAY', 'ACCEPT',
      'STOP', 'RUN', 'EXIT', 'GOBACK', 'INITIALIZE', 'INSPECT', 'REPLACING', 'STRING', 'UNSTRING',
      'CONTINUE', 'NEXT', 'SENTENCE', 'SET', 'ENTRY'
    ];
    
    const keywordPattern = new RegExp(`\\b(${keywords.join('|')})\\b`, 'g');
    highlightedCode = highlightedCode.replace(keywordPattern, '<span class="cobol-keyword">$1</span>');
    
    // Strings (in quotes)
    highlightedCode = highlightedCode.replace(
      /(['"][^'"]*['"])/g,
      '<span class="cobol-string">$1</span>'
    );
    
    // Comments (lines starting with *)
    highlightedCode = highlightedCode.replace(
      /^(\s*\*.*)$/gm,
      '<span class="cobol-comment">$1</span>'
    );
    
    // Numbers
    highlightedCode = highlightedCode.replace(
      /\b(\d+)\b/g,
      '<span class="cobol-number">$1</span>'
    );
    
    codeRef.current.innerHTML = highlightedCode;
    
    // Add line numbers
    const codeLines = code.split('\n');
    const lineNumbers = document.createElement('div');
    lineNumbers.className = 'line-numbers absolute left-0 top-0 pl-2 pr-3 text-right text-xs text-muted-foreground select-none border-r border-border';
    
    codeLines.forEach((_, index) => {
      const lineNumber = document.createElement('div');
      lineNumber.textContent = (index + 1).toString();
      
      // Highlight specific lines if requested
      if (highlightLines.includes(index + 1)) {
        lineNumber.className = 'text-primary font-medium';
      }
      
      lineNumbers.appendChild(lineNumber);
    });
    
    if (codeRef.current.parentNode) {
      codeRef.current.parentNode.insertBefore(lineNumbers, codeRef.current);
    }
  }, [code, highlightLines]);

  return (
    <div className="border rounded-lg shadow-sm bg-white dark:bg-card">
      <Tabs defaultValue="code" className="w-full">
        <div className="flex items-center justify-between px-4 py-2 border-b">
          <TabsList className="grid w-auto grid-cols-2">
            <TabsTrigger value="code" className="text-xs">Code View</TabsTrigger>
            <TabsTrigger value="raw" className="text-xs">Raw Source</TabsTrigger>
          </TabsList>
          <div className="text-xs text-muted-foreground">
            {code.split('\n').length.toLocaleString()} lines
          </div>
        </div>
        
        <TabsContent value="code" className="p-0 m-0">
          <ScrollArea className="h-[600px] code-editor-container relative">
            <div className="pl-10 py-4 pr-4 code-editor">
              <pre ref={codeRef} className="whitespace-pre overflow-visible"></pre>
            </div>
          </ScrollArea>
        </TabsContent>
        
        <TabsContent value="raw" className="p-0 m-0">
          <ScrollArea className="h-[600px]">
            <pre className="p-4 text-sm font-mono whitespace-pre-wrap overflow-auto">{code}</pre>
          </ScrollArea>
        </TabsContent>
      </Tabs>
    </div>
  );
};

export default CobolCodeViewer;
