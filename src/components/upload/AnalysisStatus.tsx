
import React from 'react';
import { Button } from '@/components/ui/button';
import { FileWarning, RotateCw } from 'lucide-react';

interface AnalysisStatusProps {
  isAnalyzing: boolean;
  onStartNewAnalysis?: () => void;
  onGoToUpload?: () => void;
}

const AnalysisStatus: React.FC<AnalysisStatusProps> = ({ 
  isAnalyzing,
  onStartNewAnalysis,
  onGoToUpload
}) => {
  if (isAnalyzing) {
    return (
      <div className="flex flex-col items-center justify-center py-24">
        <RotateCw className="h-12 w-12 text-primary animate-spin mb-4" />
        <h3 className="text-xl font-medium mb-2">Analyzing COBOL Code</h3>
        <p className="text-muted-foreground mb-6 text-center max-w-md">
          Our LLaMA 3 model is processing your code to extract business logic and documentation.
          This may take a few moments...
        </p>
        <div className="h-2 w-64 bg-muted rounded-full overflow-hidden">
          <div className="h-full bg-primary animate-pulse rounded-full"></div>
        </div>
      </div>
    );
  }

  return (
    <div className="text-center py-12">
      <FileWarning className="mx-auto h-12 w-12 text-muted-foreground mb-4" />
      <h3 className="text-lg font-medium mb-1">No Analysis Results</h3>
      <p className="text-muted-foreground mb-4">
        Please analyze a COBOL file first
      </p>
      <Button variant="outline" onClick={onGoToUpload}>
        Go to Upload
      </Button>
    </div>
  );
};

export default AnalysisStatus;
