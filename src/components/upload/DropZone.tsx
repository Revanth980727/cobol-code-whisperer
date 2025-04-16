
import React from 'react';
import { Upload, FileUp } from 'lucide-react';
import { Button } from '@/components/ui/button';

interface DropZoneProps {
  isDragging: boolean;
  onFileInput: (e: React.ChangeEvent<HTMLInputElement>) => void;
}

const DropZone: React.FC<DropZoneProps> = ({ isDragging, onFileInput }) => {
  return (
    <>
      <div className="mx-auto flex h-16 w-16 items-center justify-center rounded-full bg-muted">
        <Upload className="h-8 w-8 text-muted-foreground" />
      </div>
      <h3 className="mt-4 text-lg font-medium">Upload your COBOL code</h3>
      <p className="mt-2 text-sm text-muted-foreground">
        Drag and drop your file here, or click to browse
      </p>
      <p className="mt-1 text-xs text-muted-foreground">
        Supports .cob, .cbl, .cobol, and .cpy files
      </p>
      <div className="mt-6">
        <label htmlFor="file-upload" className="cursor-pointer">
          <Button variant="outline">
            <FileUp className="mr-2 h-4 w-4" />
            Browse files
          </Button>
          <input 
            id="file-upload" 
            type="file" 
            className="hidden" 
            accept=".cob,.cbl,.cobol,.cpy,text/plain" 
            onChange={onFileInput} 
          />
        </label>
      </div>
    </>
  );
};

export default DropZone;
