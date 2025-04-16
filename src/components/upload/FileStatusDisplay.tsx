
import React from 'react';
import { AlertCircle, CheckCircle2 } from 'lucide-react';
import { Button } from '@/components/ui/button';

interface FileStatusDisplayProps {
  file: File;
  isUploading: boolean;
  onSubmit: () => void;
  onReset: () => void;
}

const FileStatusDisplay: React.FC<FileStatusDisplayProps> = ({
  file,
  isUploading,
  onSubmit,
  onReset,
}) => {
  return (
    <>
      <div className="mx-auto flex h-16 w-16 items-center justify-center rounded-full bg-green-100 dark:bg-green-900/20">
        <CheckCircle2 className="h-8 w-8 text-green-600 dark:text-green-500" />
      </div>
      <h3 className="mt-4 text-lg font-medium">File ready to analyze</h3>
      <p className="mt-2 text-sm">
        {file.name} ({(file.size / 1024).toFixed(1)} KB)
      </p>
      
      <div className="mt-6 flex gap-2 justify-center">
        <Button
          variant="outline"
          onClick={onReset}
        >
          Change file
        </Button>
        
        <Button
          onClick={onSubmit}
          disabled={isUploading}
        >
          Analyze COBOL Code
        </Button>
      </div>
      
      <p className="mt-4 text-xs flex items-center justify-center gap-1 text-amber-600 dark:text-amber-500">
        <AlertCircle className="h-3 w-3" /> 
        Analysis may take a few moments for larger files
      </p>
    </>
  );
};

export default FileStatusDisplay;
