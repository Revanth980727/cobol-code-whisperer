
import React from 'react';
import Header from '@/components/Header';
import Footer from '@/components/Footer';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Separator } from '@/components/ui/separator';
import { ArrowRight, Check, HelpCircle, FileQuestion, Terminal, RefreshCw, ChevronDown } from 'lucide-react';
import { Accordion, AccordionContent, AccordionItem, AccordionTrigger } from '@/components/ui/accordion';

const Documentation = () => {
  return (
    <div className="min-h-screen flex flex-col">
      <Header />
      
      <div className="bg-muted py-6">
        <div className="container">
          <h1 className="text-3xl font-bold mb-2">Documentation</h1>
          <p className="text-muted-foreground">
            Learn how to use COBOL Code Whisperer to document your legacy systems
          </p>
        </div>
      </div>
      
      <main className="flex-grow py-8">
        <div className="container">
          <div className="grid grid-cols-1 md:grid-cols-4 gap-8">
            <div className="md:col-span-1">
              <div className="space-y-4 sticky top-8">
                <div className="font-medium">On this page</div>
                <ul className="space-y-2 text-sm">
                  <li>
                    <a href="#getting-started" className="text-muted-foreground hover:text-foreground transition-colors">
                      Getting Started
                    </a>
                  </li>
                  <li>
                    <a href="#upload-process" className="text-muted-foreground hover:text-foreground transition-colors">
                      Upload Process
                    </a>
                  </li>
                  <li>
                    <a href="#analysis-results" className="text-muted-foreground hover:text-foreground transition-colors">
                      Understanding Results
                    </a>
                  </li>
                  <li>
                    <a href="#feedback-loop" className="text-muted-foreground hover:text-foreground transition-colors">
                      Feedback Loop
                    </a>
                  </li>
                  <li>
                    <a href="#faq" className="text-muted-foreground hover:text-foreground transition-colors">
                      FAQ
                    </a>
                  </li>
                </ul>
              </div>
            </div>
            
            <div className="md:col-span-3 space-y-8">
              <section id="getting-started" className="scroll-mt-16">
                <h2 className="text-2xl font-bold mb-4">Getting Started</h2>
                <p className="mb-4 text-muted-foreground">
                  COBOL Code Whisperer helps you document and understand legacy COBOL systems by using AI to extract business rules and logic from source code.
                </p>
                
                <div className="bg-accent/30 rounded-lg p-6 mt-6">
                  <h3 className="text-lg font-medium mb-3">System Requirements</h3>
                  <ul className="space-y-2">
                    <li className="flex items-start gap-2">
                      <Check className="h-5 w-5 text-green-600 flex-shrink-0 mt-0.5" />
                      <span>Modern web browser (Chrome, Firefox, Safari, Edge)</span>
                    </li>
                    <li className="flex items-start gap-2">
                      <Check className="h-5 w-5 text-green-600 flex-shrink-0 mt-0.5" />
                      <span>Internet connection for web interface access</span>
                    </li>
                    <li className="flex items-start gap-2">
                      <Check className="h-5 w-5 text-green-600 flex-shrink-0 mt-0.5" />
                      <span>For local AI processing: CUDA-compatible GPU with 8GB+ VRAM</span>
                    </li>
                  </ul>
                </div>
              </section>
              
              <Separator />
              
              <section id="upload-process" className="scroll-mt-16">
                <h2 className="text-2xl font-bold mb-4">Upload Process</h2>
                <p className="mb-4 text-muted-foreground">
                  Upload your COBOL source files to begin the analysis process.
                </p>
                
                <div className="space-y-6 mt-6">
                  <div className="flex flex-col md:flex-row gap-4">
                    <div className="flex h-10 w-10 shrink-0 items-center justify-center rounded-full bg-primary text-white">
                      1
                    </div>
                    <div className="space-y-1">
                      <h3 className="text-lg font-medium">Select your COBOL source file</h3>
                      <p className="text-muted-foreground">
                        Navigate to the Upload page and either drag and drop your COBOL file or click the browse button to select it.
                      </p>
                    </div>
                  </div>
                  
                  <div className="flex flex-col md:flex-row gap-4">
                    <div className="flex h-10 w-10 shrink-0 items-center justify-center rounded-full bg-primary text-white">
                      2
                    </div>
                    <div className="space-y-1">
                      <h3 className="text-lg font-medium">Review the code</h3>
                      <p className="text-muted-foreground">
                        The uploaded code will be displayed in a syntax-highlighted viewer. Verify that your code has been uploaded correctly.
                      </p>
                    </div>
                  </div>
                  
                  <div className="flex flex-col md:flex-row gap-4">
                    <div className="flex h-10 w-10 shrink-0 items-center justify-center rounded-full bg-primary text-white">
                      3
                    </div>
                    <div className="space-y-1">
                      <h3 className="text-lg font-medium">Analyze the code</h3>
                      <p className="text-muted-foreground">
                        Click the "Analyze Code" button to start the analysis process. The system will parse the COBOL code, chunk it into sections, and process it with the LLaMA 3 model.
                      </p>
                    </div>
                  </div>
                  
                  <div className="flex flex-col md:flex-row gap-4">
                    <div className="flex h-10 w-10 shrink-0 items-center justify-center rounded-full bg-primary text-white">
                      4
                    </div>
                    <div className="space-y-1">
                      <h3 className="text-lg font-medium">Review results</h3>
                      <p className="text-muted-foreground">
                        The analysis results will be displayed, including a summary, business rules, code structure, and complexity metrics.
                      </p>
                    </div>
                  </div>
                </div>
              </section>
              
              <Separator />
              
              <section id="analysis-results" className="scroll-mt-16">
                <h2 className="text-2xl font-bold mb-4">Understanding Analysis Results</h2>
                <p className="mb-4 text-muted-foreground">
                  The analysis results provide multiple perspectives on your COBOL code.
                </p>
                
                <Tabs defaultValue="summary" className="mt-6">
                  <TabsList className="grid grid-cols-4">
                    <TabsTrigger value="summary">Summary</TabsTrigger>
                    <TabsTrigger value="business-rules">Business Rules</TabsTrigger>
                    <TabsTrigger value="code-structure">Code Structure</TabsTrigger>
                    <TabsTrigger value="complexity">Complexity</TabsTrigger>
                  </TabsList>
                  
                  <TabsContent value="summary" className="p-4 border rounded-md mt-2">
                    <h3 className="font-medium mb-2">Code Summary</h3>
                    <p className="text-muted-foreground mb-4">
                      The summary provides a high-level overview of what the COBOL program does, written in clear, modern language. This helps you quickly understand the program's purpose without diving into the details.
                    </p>
                    <div className="bg-muted p-3 rounded text-sm">
                      <span className="font-medium">Example: </span>
                      "This COBOL program (ACCTRANS) processes account transactions. It reads transaction records containing account number, transaction type (deposit or withdrawal), and amount..."
                    </div>
                  </TabsContent>
                  
                  <TabsContent value="business-rules" className="p-4 border rounded-md mt-2">
                    <h3 className="font-medium mb-2">Business Rules</h3>
                    <p className="text-muted-foreground mb-4">
                      This section extracts the key business rules and logic embedded in the COBOL code. These are the critical business requirements that the program implements.
                    </p>
                    <div className="bg-muted p-3 rounded text-sm">
                      <span className="font-medium">Examples: </span>
                      <ul className="list-disc list-inside space-y-1">
                        <li>"Transactions must be categorized as either deposits ('D') or withdrawals ('W')"</li>
                        <li>"Withdrawal transactions require a funds availability check before processing"</li>
                      </ul>
                    </div>
                  </TabsContent>
                  
                  <TabsContent value="code-structure" className="p-4 border rounded-md mt-2">
                    <h3 className="font-medium mb-2">Code Structure</h3>
                    <p className="text-muted-foreground mb-4">
                      This section breaks down the COBOL program's structure by divisions, sections, paragraphs, and other organizational elements. It helps you understand how the program is constructed.
                    </p>
                    <div className="bg-muted p-3 rounded text-sm">
                      <span className="font-medium">Example: </span>
                      <ul className="list-disc list-inside space-y-1">
                        <li>IDENTIFICATION DIVISION: Program ID, author information</li>
                        <li>DATA DIVISION: Data structures and file definitions</li>
                        <li>PROCEDURE DIVISION: Main program logic, paragraphs, and sections</li>
                      </ul>
                    </div>
                  </TabsContent>
                  
                  <TabsContent value="complexity" className="p-4 border rounded-md mt-2">
                    <h3 className="font-medium mb-2">Complexity Analysis</h3>
                    <p className="text-muted-foreground mb-4">
                      This section provides metrics about the code complexity, including cyclomatic complexity, lines of code, and comment percentage. These metrics help you understand the program's maintainability.
                    </p>
                    <div className="bg-muted p-3 rounded text-sm">
                      <span className="font-medium">Metrics: </span>
                      <ul className="list-disc list-inside space-y-1">
                        <li>Cyclomatic Complexity: Measures the number of linearly independent paths through the code</li>
                        <li>Lines of Code: Total number of lines in the program</li>
                        <li>Comment Percentage: Ratio of comment lines to total lines</li>
                      </ul>
                    </div>
                  </TabsContent>
                </Tabs>
              </section>
              
              <Separator />
              
              <section id="feedback-loop" className="scroll-mt-16">
                <h2 className="text-2xl font-bold mb-4">Feedback Loop (RFHL)</h2>
                <p className="mb-4 text-muted-foreground">
                  COBOL Code Whisperer uses Reinforcement Fine-tuning from Human Labels (RFHL) to continuously improve its analysis accuracy.
                </p>
                
                <div className="rounded-lg border bg-card p-6 mt-6">
                  <div className="flex items-center gap-4 mb-6">
                    <div className="p-3 rounded-full bg-primary/10">
                      <RefreshCw className="h-6 w-6 text-primary" />
                    </div>
                    <div>
                      <h3 className="text-lg font-bold">How the Feedback Loop Works</h3>
                      <p className="text-muted-foreground">Continuous improvement through user feedback</p>
                    </div>
                  </div>
                  
                  <ol className="space-y-4 mb-6">
                    <li className="flex gap-3">
                      <div className="flex h-6 w-6 shrink-0 items-center justify-center rounded-full bg-muted text-xs font-medium">
                        1
                      </div>
                      <div>
                        <h4 className="font-medium">Provide Feedback</h4>
                        <p className="text-sm text-muted-foreground">
                          After reviewing the analysis results, use the thumbs up/down buttons to indicate if the analysis was accurate.
                        </p>
                      </div>
                    </li>
                    <li className="flex gap-3">
                      <div className="flex h-6 w-6 shrink-0 items-center justify-center rounded-full bg-muted text-xs font-medium">
                        2
                      </div>
                      <div>
                        <h4 className="font-medium">Suggest Improvements</h4>
                        <p className="text-sm text-muted-foreground">
                          If you provide negative feedback, you'll be prompted to explain what was incorrect or missing in the analysis.
                        </p>
                      </div>
                    </li>
                    <li className="flex gap-3">
                      <div className="flex h-6 w-6 shrink-0 items-center justify-center rounded-full bg-muted text-xs font-medium">
                        3
                      </div>
                      <div>
                        <h4 className="font-medium">Model Fine-Tuning</h4>
                        <p className="text-sm text-muted-foreground">
                          Your feedback is used to fine-tune the LLaMA 3 model, improving its understanding of COBOL code over time.
                        </p>
                      </div>
                    </li>
                    <li className="flex gap-3">
                      <div className="flex h-6 w-6 shrink-0 items-center justify-center rounded-full bg-muted text-xs font-medium">
                        4
                      </div>
                      <div>
                        <h4 className="font-medium">Improved Results</h4>
                        <p className="text-sm text-muted-foreground">
                          Future analyses benefit from the accumulated feedback, creating a virtuous cycle of continuous improvement.
                        </p>
                      </div>
                    </li>
                  </ol>
                  
                  <div className="bg-muted p-4 rounded-md">
                    <div className="font-medium mb-1 flex items-center gap-2">
                      <Terminal className="h-4 w-4" />
                      Technical Note
                    </div>
                    <p className="text-sm text-muted-foreground">
                      The RFHL process uses supervised fine-tuning on the LLaMA 3 model. Your corrected summaries and explanations serve as the "gold standard" training examples that help the model align better with user expectations.
                    </p>
                  </div>
                </div>
              </section>
              
              <Separator />
              
              <section id="faq" className="scroll-mt-16">
                <h2 className="text-2xl font-bold mb-4">Frequently Asked Questions</h2>
                <p className="mb-4 text-muted-foreground">
                  Common questions about COBOL Code Whisperer.
                </p>
                
                <Accordion type="single" collapsible className="mt-6">
                  <AccordionItem value="item-1">
                    <AccordionTrigger className="hover:no-underline">
                      <div className="flex items-center gap-2">
                        <FileQuestion className="h-5 w-5 text-muted-foreground" />
                        <span>Is my COBOL code secure when uploaded?</span>
                      </div>
                    </AccordionTrigger>
                    <AccordionContent>
                      <p className="text-muted-foreground pl-7">
                        Yes. Your COBOL source code is processed locally on your machine using the LLaMA 3 model. The code is not sent to any external servers for analysis, ensuring complete privacy and security.
                      </p>
                    </AccordionContent>
                  </AccordionItem>
                  
                  <AccordionItem value="item-2">
                    <AccordionTrigger className="hover:no-underline">
                      <div className="flex items-center gap-2">
                        <FileQuestion className="h-5 w-5 text-muted-foreground" />
                        <span>What COBOL dialects are supported?</span>
                      </div>
                    </AccordionTrigger>
                    <AccordionContent>
                      <p className="text-muted-foreground pl-7">
                        COBOL Code Whisperer supports major COBOL dialects including IBM Enterprise COBOL, Micro Focus COBOL, GnuCOBOL, and COBOL85. Some vendor-specific extensions may have limited support.
                      </p>
                    </AccordionContent>
                  </AccordionItem>
                  
                  <AccordionItem value="item-3">
                    <AccordionTrigger className="hover:no-underline">
                      <div className="flex items-center gap-2">
                        <FileQuestion className="h-5 w-5 text-muted-foreground" />
                        <span>How accurate is the business logic extraction?</span>
                      </div>
                    </AccordionTrigger>
                    <AccordionContent>
                      <p className="text-muted-foreground pl-7">
                        The accuracy depends on several factors, including code complexity, documentation within the code, and the specificity of the business logic. The LLaMA 3 model has been pre-trained on COBOL examples and continuously improves through the RFHL process, but it's recommended to verify the extracted logic for critical applications.
                      </p>
                    </AccordionContent>
                  </AccordionItem>
                  
                  <AccordionItem value="item-4">
                    <AccordionTrigger className="hover:no-underline">
                      <div className="flex items-center gap-2">
                        <FileQuestion className="h-5 w-5 text-muted-foreground" />
                        <span>Can I analyze copybooks separately?</span>
                      </div>
                    </AccordionTrigger>
                    <AccordionContent>
                      <p className="text-muted-foreground pl-7">
                        Yes, you can upload and analyze COBOL copybooks (.cpy files) separately. The system will analyze the structure and purpose of the copybook, though some context that would be available in the main program might be missing.
                      </p>
                    </AccordionContent>
                  </AccordionItem>
                  
                  <AccordionItem value="item-5">
                    <AccordionTrigger className="hover:no-underline">
                      <div className="flex items-center gap-2">
                        <FileQuestion className="h-5 w-5 text-muted-foreground" />
                        <span>What happens to my feedback?</span>
                      </div>
                    </AccordionTrigger>
                    <AccordionContent>
                      <p className="text-muted-foreground pl-7">
                        Your feedback is used to fine-tune the LLaMA 3 model using the RFHL process. Positive feedback helps reinforce correct analyses, while negative feedback with explanations helps correct the model's understanding. The feedback data is stored locally and only used for model improvement.
                      </p>
                    </AccordionContent>
                  </AccordionItem>
                  
                  <AccordionItem value="item-6">
                    <AccordionTrigger className="hover:no-underline">
                      <div className="flex items-center gap-2">
                        <FileQuestion className="h-5 w-5 text-muted-foreground" />
                        <span>Can I export the documentation?</span>
                      </div>
                    </AccordionTrigger>
                    <AccordionContent>
                      <p className="text-muted-foreground pl-7">
                        Yes, the analysis results can be exported in multiple formats including PDF, HTML, and Markdown. This feature allows you to save, share, and incorporate the generated documentation into your existing documentation system.
                      </p>
                    </AccordionContent>
                  </AccordionItem>
                </Accordion>
              </section>
            </div>
          </div>
        </div>
      </main>
      
      <Footer />
    </div>
  );
};

export default Documentation;
