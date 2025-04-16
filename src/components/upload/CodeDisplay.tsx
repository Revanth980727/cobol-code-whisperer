
import React from 'react';
import { Button } from '@/components/ui/button';
import { FileWarning, Loader2 } from 'lucide-react';
import CobolCodeViewer from '@/components/CobolCodeViewer';

interface CodeDisplayProps {
  file: File | null;
  cobolCode: string;
  isAnalyzing: boolean;
  hasAnalysisResults: boolean;
  onAnalyzeCode: () => void;
  onViewAnalysis: () => void;
  onGoToUpload: () => void;
}

const CodeDisplay: React.FC<CodeDisplayProps> = ({
  file,
  cobolCode,
  isAnalyzing,
  hasAnalysisResults,
  onAnalyzeCode,
  onViewAnalysis,
  onGoToUpload
}) => {
  if (!cobolCode) {
    return (
      <div className="text-center py-12">
        <FileWarning className="mx-auto h-12 w-12 text-muted-foreground mb-4" />
        <h3 className="text-lg font-medium mb-1">No COBOL Code Available</h3>
        <p className="text-muted-foreground mb-4">
          Please upload a COBOL source file first
        </p>
        <Button variant="outline" onClick={onGoToUpload}>
          Go to Upload
        </Button>
      </div>
    );
  }

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div>
          <h2 className="text-xl font-semibold">{file?.name}</h2>
          <p className="text-sm text-muted-foreground">
            {(file?.size / 1024).toFixed(1)} KB • {cobolCode.split('\n').length} lines
          </p>
        </div>
        
        {isAnalyzing ? (
          <Button disabled>
            <Loader2 className="mr-2 h-4 w-4 animate-spin" />
            Analyzing...
          </Button>
        ) : hasAnalysisResults ? (
          <Button onClick={onViewAnalysis}>
            View Analysis Results
          </Button>
        ) : (
          <Button onClick={onAnalyzeCode}>
            Analyze Code
          </Button>
        )}
      </div>
      
      <CobolCodeViewer code={cobolCode} />
    </div>
  );
};

export default CodeDisplay;
