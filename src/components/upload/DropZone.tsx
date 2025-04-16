
import React, { useRef } from 'react';
import { Upload, FileUp } from 'lucide-react';
import { Button } from '@/components/ui/button';

interface DropZoneProps {
  isDragging: boolean;
  onFileInput: (e: React.ChangeEvent<HTMLInputElement>) => void;
}

const DropZone: React.FC<DropZoneProps> = ({ isDragging, onFileInput }) => {
  const fileInputRef = useRef<HTMLInputElement>(null);
  
  const handleBrowseClick = () => {
    // Trigger the hidden file input click when the button is clicked
    if (fileInputRef.current) {
      fileInputRef.current.click();
    }
  };
  
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
        <Button 
          variant="outline" 
          onClick={handleBrowseClick}
        >
          <FileUp className="mr-2 h-4 w-4" />
          Browse files
        </Button>
        <input 
          id="file-upload" 
          ref={fileInputRef}
          type="file" 
          className="hidden" 
          accept=".cob,.cbl,.cobol,.cpy,text/plain" 
          onChange={onFileInput} 
        />
      </div>
    </>
  );
};

export default DropZone;

