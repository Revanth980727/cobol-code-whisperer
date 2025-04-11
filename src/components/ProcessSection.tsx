
import React from 'react';
import { 
  Upload, 
  FileSearch, 
  Cpu, 
  FileText, 
  ThumbsUp
} from 'lucide-react';

const ProcessSection = () => {
  const steps = [
    {
      icon: <Upload className="h-8 w-8 text-white" />,
      title: "Upload COBOL Code",
      description: "Upload your legacy COBOL source files securely to the application.",
      color: "bg-blue-500"
    },
    {
      icon: <FileSearch className="h-8 w-8 text-white" />,
      title: "Code Parsing & Chunking",
      description: "The system breaks down the COBOL code into manageable segments for analysis.",
      color: "bg-purple-500"
    },
    {
      icon: <Cpu className="h-8 w-8 text-white" />,
      title: "LLaMA 3 Processing",
      description: "Our locally-running LLaMA 3 model analyzes each code segment to extract meaning.",
      color: "bg-green-500"
    },
    {
      icon: <FileText className="h-8 w-8 text-white" />,
      title: "Documentation Generation",
      description: "The system produces clear documentation and identifies business logic.",
      color: "bg-amber-500"
    },
    {
      icon: <ThumbsUp className="h-8 w-8 text-white" />,
      title: "Human Feedback & Fine-tuning",
      description: "Your feedback improves the model's understanding of COBOL code over time.",
      color: "bg-red-500"
    }
  ];

  return (
    <section className="py-16 bg-white dark:bg-background">
      <div className="container">
        <div className="text-center mb-12">
          <h2 className="text-3xl font-bold tracking-tight mb-2">How It Works</h2>
          <p className="text-muted-foreground max-w-2xl mx-auto">
            Our process combines advanced code analysis with AI-powered documentation generation
          </p>
        </div>
        
        <div className="relative">
          {/* Connector line */}
          <div className="absolute top-16 left-[calc(50%)] h-[calc(100%-80px)] w-0.5 bg-gradient-to-b from-blue-500 to-red-500 hidden md:block"></div>
          
          <div className="space-y-12 relative">
            {steps.map((step, index) => (
              <div 
                key={index}
                className={`flex flex-col md:flex-row ${
                  index % 2 === 0 ? 'md:flex-row-reverse' : ''
                } md:items-center gap-6`}
              >
                <div className="md:w-1/2 flex justify-center">
                  <div className={`${step.color} w-20 h-20 rounded-full flex items-center justify-center shadow-lg relative z-10`}>
                    {step.icon}
                    <div className="absolute -bottom-1 -right-1 bg-white dark:bg-background text-cobol-blue w-8 h-8 rounded-full flex items-center justify-center font-bold shadow-sm">
                      {index + 1}
                    </div>
                  </div>
                </div>
                
                <div className="md:w-1/2">
                  <h3 className="text-xl font-bold mb-2">{step.title}</h3>
                  <p className="text-muted-foreground">{step.description}</p>
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>
    </section>
  );
};

export default ProcessSection;
