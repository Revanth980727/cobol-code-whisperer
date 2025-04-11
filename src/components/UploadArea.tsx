
import React, { useState } from 'react';
import { Upload, FileUp, AlertCircle, RotateCw, CheckCircle2 } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { toast } from 'sonner';

interface UploadAreaProps {
  onFileUpload: (file: File) => void;
  isUploading: boolean;
}

const UploadArea: React.FC<UploadAreaProps> = ({ onFileUpload, isUploading }) => {
  const [isDragging, setIsDragging] = useState(false);
  const [file, setFile] = useState<File | null>(null);

  const handleDragOver = (e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    setIsDragging(true);
  };

  const handleDragLeave = () => {
    setIsDragging(false);
  };

  const handleDrop = (e: React.DragEvent<HTMLDivElement>) => {
    e.preventDefault();
    setIsDragging(false);
    
    if (e.dataTransfer.files && e.dataTransfer.files.length > 0) {
      const uploadedFile = e.dataTransfer.files[0];
      validateAndSetFile(uploadedFile);
    }
  };

  const handleFileInput = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (e.target.files && e.target.files.length > 0) {
      const uploadedFile = e.target.files[0];
      validateAndSetFile(uploadedFile);
    }
  };

  const validateAndSetFile = (uploadedFile: File) => {
    // Check if file is likely a COBOL file (by extension or content check)
    const fileName = uploadedFile.name.toLowerCase();
    const isCobolFile = fileName.endsWith('.cob') || 
                         fileName.endsWith('.cbl') || 
                         fileName.endsWith('.cobol') ||
                         fileName.endsWith('.cpy');
    
    if (!isCobolFile) {
      toast.warning("The file doesn't appear to be a COBOL file. Proceed with caution.", {
        description: "We recommend using files with .cob, .cbl, .cobol or .cpy extensions."
      });
    }
    
    if (uploadedFile.size > 5 * 1024 * 1024) {
      toast.error("File is too large", { 
        description: "Maximum file size is 5MB." 
      });
      return;
    }
    
    setFile(uploadedFile);
    toast.success("File selected", { 
      description: `${uploadedFile.name} (${(uploadedFile.size / 1024).toFixed(1)} KB)` 
    });
  };

  const handleSubmit = () => {
    if (file) {
      onFileUpload(file);
    }
  };

  return (
    <div 
      className={`border-2 border-dashed rounded-lg p-8 text-center transition-all ${
        isDragging 
          ? 'border-primary bg-primary/5' 
          : file 
            ? 'border-green-500 bg-green-50 dark:bg-green-900/10' 
            : 'border-muted-foreground/30'
      }`}
      onDragOver={handleDragOver}
      onDragLeave={handleDragLeave}
      onDrop={handleDrop}
    >
      {!file ? (
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
                onChange={handleFileInput} 
              />
            </label>
          </div>
        </>
      ) : (
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
              onClick={() => setFile(null)}
            >
              Change file
            </Button>
            
            <Button
              onClick={handleSubmit}
              disabled={isUploading}
            >
              {isUploading ? (
                <>
                  <RotateCw className="mr-2 h-4 w-4 animate-spin" />
                  Uploading...
                </>
              ) : (
                <>
                  Analyze COBOL Code
                </>
              )}
            </Button>
          </div>
          
          <p className="mt-4 text-xs flex items-center justify-center gap-1 text-amber-600 dark:text-amber-500">
            <AlertCircle className="h-3 w-3" /> 
            Analysis may take a few moments for larger files
          </p>
        </>
      )}
    </div>
  );
};

export default UploadArea;
