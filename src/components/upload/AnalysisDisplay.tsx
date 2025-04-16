
import React from 'react';
import { Button } from '@/components/ui/button';
import AnalysisResults from '@/components/AnalysisResults';
import { AnalysisResult } from '@/services/api';
import AnalysisStatus from './AnalysisStatus';

interface AnalysisDisplayProps {
  file: File | null;
  cobolCode: string;
  isAnalyzing: boolean;
  analysisResults: AnalysisResult | null;
  onBackToCode: () => void;
  onResetAnalysis: () => void;
}

const AnalysisDisplay: React.FC<AnalysisDisplayProps> = ({
  file,
  cobolCode,
  isAnalyzing,
  analysisResults,
  onBackToCode,
  onResetAnalysis,
}) => {
  if (isAnalyzing || !analysisResults) {
    return <AnalysisStatus isAnalyzing={isAnalyzing} />;
  }

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div>
          <h2 className="text-xl font-semibold">Analysis Results</h2>
          <p className="text-sm text-muted-foreground">
            {file?.name} • {cobolCode.split('\n').length} lines of code
          </p>
        </div>
        
        <div className="flex gap-2">
          <Button variant="outline" onClick={onBackToCode}>
            Back to Code
          </Button>
          <Button variant="outline" onClick={onResetAnalysis}>
            Start New Analysis
          </Button>
        </div>
      </div>
      
      <AnalysisResults 
        analysisData={{
          summary: analysisResults.summary,
          businessRules: analysisResults.business_rules,
          codeStructure: analysisResults.code_structure,
          complexity: analysisResults.complexity,
          callGraph: analysisResults.call_graph,
          dataFlow: analysisResults.data_flow,
          chunks: analysisResults.chunks
        }} 
      />
    </div>
  );
};

export default AnalysisDisplay;
