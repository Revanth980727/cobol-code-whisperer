
import React, { useState } from 'react';
import { toast } from 'sonner';
import DropZone from './upload/DropZone';
import FileStatusDisplay from './upload/FileStatusDisplay';

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
        <DropZone 
          isDragging={isDragging}
          onFileInput={handleFileInput}
        />
      ) : (
        <FileStatusDisplay
          file={file}
          isUploading={isUploading}
          onSubmit={handleSubmit}
          onReset={() => setFile(null)}
        />
      )}
    </div>
  );
};

export default UploadArea;
