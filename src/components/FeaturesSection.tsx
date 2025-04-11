
import React from 'react';
import { 
  FileText, 
  Sparkles, 
  Brain, 
  RefreshCw, 
  Code2, 
  ThumbsUp, 
  Layers,
  BookOpen
} from 'lucide-react';

const FeaturesSection = () => {
  const features = [
    {
      icon: <FileText className="h-10 w-10 text-primary" />,
      title: "COBOL Documentation",
      description: "Transform legacy COBOL code into clear, modern documentation that anyone can understand."
    },
    {
      icon: <Sparkles className="h-10 w-10 text-amber-500" />,
      title: "Business Logic Extraction",
      description: "Identify and explain key business rules hidden in decades-old COBOL programs."
    },
    {
      icon: <Brain className="h-10 w-10 text-violet-500" />,
      title: "Local LLaMA 3 Model",
      description: "Run analysis securely on your own hardware with an optimized large language model."
    },
    {
      icon: <RefreshCw className="h-10 w-10 text-green-500" />,
      title: "RFHL Fine-tuning",
      description: "The system continuously improves through Reinforcement Fine-tuning from Human Labels."
    },
    {
      icon: <Layers className="h-10 w-10 text-blue-500" />,
      title: "Code Structure Analysis",
      description: "Visualize and understand the organization of COBOL programs, divisions, and sections."
    },
    {
      icon: <Code2 className="h-10 w-10 text-cobol-blue" />,
      title: "Legacy Code Support",
      description: "Handle various COBOL dialects, including complex mainframe-specific syntax."
    },
    {
      icon: <ThumbsUp className="h-10 w-10 text-rose-500" />,
      title: "Human Feedback Loop",
      description: "Provide feedback to continuously improve the AI's understanding of your codebase."
    },
    {
      icon: <BookOpen className="h-10 w-10 text-teal-500" />,
      title: "Knowledge Preservation",
      description: "Capture the implicit knowledge in legacy systems before it's lost to retirement."
    }
  ];

  return (
    <section className="py-16 bg-accent/30">
      <div className="container">
        <div className="text-center mb-12">
          <h2 className="text-3xl font-bold tracking-tight mb-2">Powerful Features</h2>
          <p className="text-muted-foreground max-w-2xl mx-auto">
            Our tool combines advanced AI with domain-specific COBOL knowledge to unlock your legacy code.
          </p>
        </div>
        
        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-6">
          {features.map((feature, index) => (
            <div 
              key={index} 
              className="p-6 bg-card shadow-sm rounded-lg border hover:shadow-md transition-shadow"
            >
              <div className="mb-4">
                {feature.icon}
              </div>
              <h3 className="text-lg font-medium mb-2">{feature.title}</h3>
              <p className="text-sm text-muted-foreground">{feature.description}</p>
            </div>
          ))}
        </div>
      </div>
    </section>
  );
};

export default FeaturesSection;
