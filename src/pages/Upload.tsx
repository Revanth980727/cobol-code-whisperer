
import React, { useState, useEffect } from 'react';
import Header from '@/components/Header';
import Footer from '@/components/Footer';
import UploadArea from '@/components/UploadArea';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { toast } from 'sonner';
import { analyzeCobolFile, AnalysisResult, checkModelStatus, ModelStatus as ModelStatusType } from '@/services/api';
import ModelStatusIndicator from '@/components/upload/ModelStatus';
import CodeDisplay from '@/components/upload/CodeDisplay';
import AnalysisDisplay from '@/components/upload/AnalysisDisplay';

const Upload = () => {
  const [file, setFile] = useState<File | null>(null);
  const [isUploading, setIsUploading] = useState(false);
  const [isAnalyzing, setIsAnalyzing] = useState(false);
  const [cobolCode, setCobolCode] = useState<string>('');
  const [analysisResults, setAnalysisResults] = useState<AnalysisResult | null>(null);
  const [activeTab, setActiveTab] = useState<string>('upload');
  const [modelStatus, setModelStatus] = useState<ModelStatusType | null>(null);
  const [modelChecked, setModelChecked] = useState(false);

  useEffect(() => {
    const checkLLM = async () => {
      try {
        const status = await checkModelStatus();
        setModelStatus(status);
      } catch (error) {
        console.error("Failed to check model status:", error);
      } finally {
        setModelChecked(true);
      }
    };
    
    checkLLM();
  }, []);

  const handleFileUpload = (uploadedFile: File) => {
    setIsUploading(true);
    setFile(uploadedFile);
    
    const reader = new FileReader();
    reader.onload = (e) => {
      const content = e.target?.result as string;
      setCobolCode(content);
      setIsUploading(false);
      setActiveTab('code');
      
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
    if (modelStatus?.status !== "ready") {
      toast.warning("LLaMA 3 model may not be fully loaded", {
        description: "Analysis might use fallback methods or have reduced accuracy."
      });
    }
    
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
            
            <ModelStatusIndicator 
              modelStatus={modelStatus} 
              modelChecked={modelChecked} 
            />
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
              <CodeDisplay
                file={file}
                cobolCode={cobolCode}
                isAnalyzing={isAnalyzing}
                hasAnalysisResults={!!analysisResults}
                onAnalyzeCode={() => handleAnalyzeCode(file!)}
                onViewAnalysis={() => setActiveTab('analysis')}
                onGoToUpload={() => setActiveTab('upload')}
              />
            </TabsContent>
            
            <TabsContent value="analysis" className="p-0 m-0">
              <AnalysisDisplay
                file={file}
                cobolCode={cobolCode}
                isAnalyzing={isAnalyzing}
                analysisResults={analysisResults}
                onBackToCode={() => setActiveTab('code')}
                onResetAnalysis={resetAnalysis}
              />
            </TabsContent>
          </Tabs>
        </div>
      </main>
      
      <Footer />
    </div>
  );
};

export default Upload;
