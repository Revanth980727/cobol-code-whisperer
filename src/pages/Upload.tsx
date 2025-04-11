
import React, { useState } from 'react';
import Header from '@/components/Header';
import Footer from '@/components/Footer';
import UploadArea from '@/components/UploadArea';
import CobolCodeViewer from '@/components/CobolCodeViewer';
import AnalysisResults from '@/components/AnalysisResults';
import { Button } from '@/components/ui/button';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { toast } from 'sonner';
import { Loader2, FileWarning, RotateCw } from 'lucide-react';
import { analyzeCobolFile, AnalysisResult } from '@/services/api';

const Upload = () => {
  const [file, setFile] = useState<File | null>(null);
  const [isUploading, setIsUploading] = useState(false);
  const [isAnalyzing, setIsAnalyzing] = useState(false);
  const [cobolCode, setCobolCode] = useState<string>('');
  const [analysisResults, setAnalysisResults] = useState<AnalysisResult | null>(null);
  const [activeTab, setActiveTab] = useState<string>('upload');

  const handleFileUpload = (uploadedFile: File) => {
    setIsUploading(true);
    setFile(uploadedFile);
    
    // Read the file content for display
    const reader = new FileReader();
    reader.onload = (e) => {
      const content = e.target?.result as string;
      setCobolCode(content);
      setIsUploading(false);
      setActiveTab('code');
      
      // Begin analysis
      handleAnalyzeCode(uploadedFile);
    };
    reader.onerror = () => {
      setIsUploading(false);
      toast.error("Failed to read file", {
        description: "Please check if the file is valid and try again."
      });
    };
    
    reader.readAsText(uploadedFile);
  };

  const handleAnalyzeCode = async (fileToAnalyze: File) => {
    setIsAnalyzing(true);
    
    try {
      const result = await analyzeCobolFile(fileToAnalyze);
      setAnalysisResults(result);
      setActiveTab('analysis');
      
      toast.success("Analysis complete", {
        description: "View the generated documentation and provide feedback to improve results."
      });
    } catch (error) {
      toast.error("Analysis failed", {
        description: "There was an error analyzing your COBOL file. Please try again."
      });
      console.error("Analysis error:", error);
    } finally {
      setIsAnalyzing(false);
    }
  };

  const resetAnalysis = () => {
    setFile(null);
    setCobolCode('');
    setAnalysisResults(null);
    setActiveTab('upload');
  };

  return (
    <div className="min-h-screen flex flex-col">
      <Header />
      
      <main className="flex-grow py-8">
        <div className="container">
          <div className="mb-6">
            <h1 className="text-3xl font-bold mb-2">COBOL Code Analysis</h1>
            <p className="text-muted-foreground">
              Upload your COBOL source files to generate documentation and extract business logic
            </p>
          </div>
          
          <Tabs value={activeTab} onValueChange={setActiveTab} className="space-y-6">
            <TabsList className="grid grid-cols-3 w-full max-w-md mb-4">
              <TabsTrigger value="upload">Upload</TabsTrigger>
              <TabsTrigger value="code" disabled={!cobolCode}>Code</TabsTrigger>
              <TabsTrigger value="analysis" disabled={!analysisResults}>Results</TabsTrigger>
            </TabsList>
            
            <TabsContent value="upload" className="p-0 m-0">
              <div className="max-w-3xl mx-auto">
                <UploadArea 
                  onFileUpload={handleFileUpload} 
                  isUploading={isUploading} 
                />
              </div>
            </TabsContent>
            
            <TabsContent value="code" className="p-0 m-0">
              {cobolCode ? (
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
                    ) : analysisResults ? (
                      <Button onClick={() => setActiveTab('analysis')}>
                        View Analysis Results
                      </Button>
                    ) : (
                      <Button onClick={() => handleAnalyzeCode(file!)}>
                        Analyze Code
                      </Button>
                    )}
                  </div>
                  
                  <CobolCodeViewer code={cobolCode} />
                </div>
              ) : (
                <div className="text-center py-12">
                  <FileWarning className="mx-auto h-12 w-12 text-muted-foreground mb-4" />
                  <h3 className="text-lg font-medium mb-1">No COBOL Code Available</h3>
                  <p className="text-muted-foreground mb-4">
                    Please upload a COBOL source file first
                  </p>
                  <Button variant="outline" onClick={() => setActiveTab('upload')}>
                    Go to Upload
                  </Button>
                </div>
              )}
            </TabsContent>
            
            <TabsContent value="analysis" className="p-0 m-0">
              {isAnalyzing ? (
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
              ) : analysisResults ? (
                <div className="space-y-6">
                  <div className="flex items-center justify-between">
                    <div>
                      <h2 className="text-xl font-semibold">Analysis Results</h2>
                      <p className="text-sm text-muted-foreground">
                        {file?.name} • {cobolCode.split('\n').length} lines of code
                      </p>
                    </div>
                    
                    <div className="flex gap-2">
                      <Button variant="outline" onClick={() => setActiveTab('code')}>
                        Back to Code
                      </Button>
                      <Button variant="outline" onClick={resetAnalysis}>
                        Start New Analysis
                      </Button>
                    </div>
                  </div>
                  
                  <AnalysisResults analysisData={analysisResults} />
                </div>
              ) : (
                <div className="text-center py-12">
                  <FileWarning className="mx-auto h-12 w-12 text-muted-foreground mb-4" />
                  <h3 className="text-lg font-medium mb-1">No Analysis Results</h3>
                  <p className="text-muted-foreground mb-4">
                    Please analyze a COBOL file first
                  </p>
                  <Button variant="outline" onClick={() => setActiveTab('upload')}>
                    Go to Upload
                  </Button>
                </div>
              )}
            </TabsContent>
          </Tabs>
        </div>
      </main>
      
      <Footer />
    </div>
  );
};

export default Upload;
