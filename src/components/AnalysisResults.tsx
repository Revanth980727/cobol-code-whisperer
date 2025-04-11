
import React from 'react';
import { Card, CardHeader, CardTitle, CardDescription, CardContent } from '@/components/ui/card';
import { Accordion, AccordionContent, AccordionItem, AccordionTrigger } from '@/components/ui/accordion';
import { Badge } from '@/components/ui/badge';
import { ScrollArea } from '@/components/ui/scroll-area';
import { FileText, ArrowRight, ThumbsUp, ThumbsDown, AlertCircle, Sparkles, Layers, Code2 } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Textarea } from '@/components/ui/textarea';
import { toast } from 'sonner';

export interface AnalysisResultsProps {
  analysisData: {
    summary: string;
    businessRules: string[];
    codeStructure: {
      division: string;
      elements: { name: string; description: string }[];
    }[];
    complexity: {
      cyclomatic: number;
      linesOfCode: number;
      commentPercentage: number;
    };
  };
}

const AnalysisResults: React.FC<AnalysisResultsProps> = ({ analysisData }) => {
  const { summary, businessRules, codeStructure, complexity } = analysisData;

  const handleFeedback = (positive: boolean) => {
    if (positive) {
      toast.success("Positive feedback submitted", {
        description: "Thank you for helping us improve our analysis."
      });
    } else {
      toast.custom((id) => (
        <div className="max-w-md w-full bg-white shadow-lg rounded-lg pointer-events-auto flex flex-col dark:bg-gray-900 dark:border dark:border-gray-700">
          <div className="p-4 flex items-start">
            <div className="flex-shrink-0 text-red-500 dark:text-red-400">
              <AlertCircle className="h-6 w-6" />
            </div>
            <div className="ml-3 w-0 flex-1">
              <p className="text-sm font-medium text-gray-900 dark:text-gray-100">
                Help us improve
              </p>
              <p className="mt-1 text-sm text-gray-500 dark:text-gray-400">
                Please tell us how we can improve this analysis.
              </p>
              <div className="mt-3">
                <Textarea 
                  placeholder="What was incorrect or missing?" 
                  className="w-full text-sm"
                  rows={3}
                />
              </div>
              <div className="mt-3 flex gap-2">
                <Button 
                  size="sm" 
                  onClick={() => toast.dismiss(id)}
                >
                  Submit Feedback
                </Button>
                <Button 
                  size="sm" 
                  variant="outline" 
                  onClick={() => toast.dismiss(id)}
                >
                  Cancel
                </Button>
              </div>
            </div>
          </div>
        </div>
      ));
    }
  };

  const getComplexityLevel = (score: number) => {
    if (score < 10) return { label: "Low", color: "bg-green-500" };
    if (score < 20) return { label: "Moderate", color: "bg-yellow-500" };
    return { label: "High", color: "bg-red-500" };
  };
  
  const complexityLevel = getComplexityLevel(complexity.cyclomatic);

  return (
    <div className="space-y-6 animate-fade-in">
      <Card>
        <CardHeader className="pb-3">
          <div className="flex items-center justify-between">
            <CardTitle className="text-xl flex items-center gap-2">
              <FileText className="h-5 w-5 text-primary" />
              Code Analysis Summary
            </CardTitle>
            <div className="flex gap-2">
              <Button
                variant="outline"
                size="sm"
                className="h-8 gap-1"
                onClick={() => handleFeedback(true)}
              >
                <ThumbsUp className="h-3.5 w-3.5" />
                <span className="sr-only sm:not-sr-only sm:inline">Helpful</span>
              </Button>
              <Button
                variant="outline"
                size="sm"
                className="h-8 gap-1"
                onClick={() => handleFeedback(false)}
              >
                <ThumbsDown className="h-3.5 w-3.5" />
                <span className="sr-only sm:not-sr-only sm:inline">Not Helpful</span>
              </Button>
            </div>
          </div>
          <CardDescription>
            AI-generated documentation based on your COBOL code
          </CardDescription>
        </CardHeader>
        <CardContent>
          <ScrollArea className="h-[200px] rounded-md border p-4">
            <div className="prose dark:prose-invert max-w-none">
              <p>{summary}</p>
            </div>
          </ScrollArea>
        </CardContent>
      </Card>

      <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
        <Card className="md:col-span-2">
          <CardHeader>
            <CardTitle className="text-lg flex items-center gap-2">
              <Sparkles className="h-5 w-5 text-yellow-500" />
              Business Rules
            </CardTitle>
            <CardDescription>
              Key business logic identified in the code
            </CardDescription>
          </CardHeader>
          <CardContent>
            <ScrollArea className="h-[300px]">
              <ul className="space-y-3">
                {businessRules.map((rule, index) => (
                  <li key={index} className="flex gap-2">
                    <ArrowRight className="h-5 w-5 flex-shrink-0 text-primary mt-0.5" />
                    <span>{rule}</span>
                  </li>
                ))}
              </ul>
            </ScrollArea>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle className="text-lg flex items-center gap-2">
              <Code2 className="h-5 w-5 text-primary" />
              Complexity Analysis
            </CardTitle>
            <CardDescription>
              Metrics about the code structure
            </CardDescription>
          </CardHeader>
          <CardContent>
            <div className="space-y-4">
              <div className="flex flex-col gap-1">
                <span className="text-sm font-medium">Cyclomatic Complexity</span>
                <div className="flex items-center gap-2">
                  <span className="text-xl font-bold">{complexity.cyclomatic}</span>
                  <Badge variant="outline" className={`${complexityLevel.color} text-white`}>
                    {complexityLevel.label}
                  </Badge>
                </div>
              </div>
              
              <div className="flex flex-col gap-1">
                <span className="text-sm font-medium">Lines of Code</span>
                <span className="text-xl font-bold">{complexity.linesOfCode.toLocaleString()}</span>
              </div>
              
              <div className="flex flex-col gap-1">
                <span className="text-sm font-medium">Comment Percentage</span>
                <div className="flex items-center gap-2">
                  <span className="text-xl font-bold">{complexity.commentPercentage}%</span>
                  <div className="w-full bg-muted rounded-full h-2">
                    <div 
                      className="bg-primary h-2 rounded-full" 
                      style={{ width: `${complexity.commentPercentage}%` }}
                    ></div>
                  </div>
                </div>
              </div>
            </div>
          </CardContent>
        </Card>
      </div>

      <Card>
        <CardHeader>
          <CardTitle className="text-lg flex items-center gap-2">
            <Layers className="h-5 w-5 text-primary" />
            Code Structure
          </CardTitle>
          <CardDescription>
            Program organization and key components
          </CardDescription>
        </CardHeader>
        <CardContent>
          <Accordion type="single" collapsible className="w-full">
            {codeStructure.map((division, index) => (
              <AccordionItem key={index} value={`division-${index}`}>
                <AccordionTrigger className="font-medium">
                  {division.division}
                </AccordionTrigger>
                <AccordionContent>
                  <div className="space-y-2 pl-4">
                    {division.elements.map((element, elementIndex) => (
                      <div key={elementIndex} className="border-l-2 border-muted pl-4 py-2">
                        <div className="font-medium">{element.name}</div>
                        <div className="text-sm text-muted-foreground">
                          {element.description}
                        </div>
                      </div>
                    ))}
                  </div>
                </AccordionContent>
              </AccordionItem>
            ))}
          </Accordion>
        </CardContent>
      </Card>
    </div>
  );
};

export default AnalysisResults;
