
import React from 'react';
import Header from '@/components/Header';
import Footer from '@/components/Footer';
import { Separator } from '@/components/ui/separator';
import { Card, CardContent } from '@/components/ui/card';
import { Terminal, BookOpen, Zap, MoveRight, Github, Brain } from 'lucide-react';
import { Link } from 'react-router-dom';
import { Button } from '@/components/ui/button';

const About = () => {
  return (
    <div className="min-h-screen flex flex-col">
      <Header />
      
      <main className="flex-grow py-8">
        <div className="container">
          <div className="max-w-3xl mx-auto">
            <div className="text-center mb-12">
              <div className="inline-flex items-center justify-center p-3 bg-primary/10 rounded-full mb-4">
                <Terminal className="h-8 w-8 text-primary" />
              </div>
              <h1 className="text-4xl font-bold mb-4">About COBOL Code Whisperer</h1>
              <p className="text-xl text-muted-foreground">
                Preserving legacy knowledge with modern AI technology
              </p>
            </div>
            
            <div className="prose dark:prose-invert max-w-none mb-12">
              <p className="lead">
                COBOL Code Whisperer is a specialized tool designed to bridge the gap between decades-old legacy code and modern development practices. Using advanced AI techniques, it transforms complex COBOL codebases into clear, accessible documentation.
              </p>
              
              <h2>Our Mission</h2>
              
              <p>
                As organizations struggle with maintaining critical COBOL systems while facing a diminishing pool of COBOL expertise, our mission is to capture, preserve, and transfer the business knowledge embedded in legacy code. By making COBOL systems understandable to developers regardless of their COBOL expertise, we help organizations:
              </p>
              
              <ul>
                <li>Mitigate risks associated with retiring COBOL experts</li>
                <li>Accelerate onboarding of new developers to legacy systems</li>
                <li>Facilitate modernization and migration initiatives</li>
                <li>Improve the maintainability of critical business applications</li>
              </ul>
              
              <h2>Technology</h2>
              
              <p>
                COBOL Code Whisperer combines several cutting-edge technologies:
              </p>
              
              <h3>Local LLaMA 3 Model</h3>
              <p>
                We utilize the powerful LLaMA 3 large language model, fine-tuned specifically for COBOL code understanding. By running the model locally on your hardware, we ensure complete privacy and security of your proprietary code.
              </p>
              
              <h3>RFHL (Reinforcement Fine-tuning from Human Labels)</h3>
              <p>
                Our system continuously improves through user feedback. When you provide corrections or validation of the AI's analysis, those inputs are used to fine-tune the model, making it increasingly accurate over time.
              </p>
              
              <h3>Specialized COBOL Parser</h3>
              <p>
                We've implemented a COBOL-specific parsing engine that understands the unique syntax and structure of COBOL programs, including various dialects and extensions commonly found in enterprise systems.
              </p>
            </div>
            
            <Separator className="my-12" />
            
            <div className="mb-12">
              <h2 className="text-3xl font-bold mb-6 text-center">Key Benefits</h2>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <Card>
                  <CardContent className="pt-6">
                    <div className="mb-4 flex items-center gap-2">
                      <BookOpen className="h-5 w-5 text-primary" />
                      <h3 className="font-bold">Knowledge Preservation</h3>
                    </div>
                    <p className="text-muted-foreground">
                      Capture critical business knowledge from legacy code before it's lost to retirement or system replacement.
                    </p>
                  </CardContent>
                </Card>
                
                <Card>
                  <CardContent className="pt-6">
                    <div className="mb-4 flex items-center gap-2">
                      <Zap className="h-5 w-5 text-primary" />
                      <h3 className="font-bold">Accelerated Onboarding</h3>
                    </div>
                    <p className="text-muted-foreground">
                      Help new developers quickly understand complex COBOL systems without extensive COBOL training.
                    </p>
                  </CardContent>
                </Card>
                
                <Card>
                  <CardContent className="pt-6">
                    <div className="mb-4 flex items-center gap-2">
                      <Brain className="h-5 w-5 text-primary" />
                      <h3 className="font-bold">Privacy & Security</h3>
                    </div>
                    <p className="text-muted-foreground">
                      Process all code locally, ensuring sensitive business logic never leaves your infrastructure.
                    </p>
                  </CardContent>
                </Card>
                
                <Card>
                  <CardContent className="pt-6">
                    <div className="mb-4 flex items-center gap-2">
                      <Github className="h-5 w-5 text-primary" />
                      <h3 className="font-bold">Open Development</h3>
                    </div>
                    <p className="text-muted-foreground">
                      Built on open-source technologies, with a transparent approach to continuous improvement.
                    </p>
                  </CardContent>
                </Card>
              </div>
            </div>
            
            <div className="bg-primary/5 rounded-lg p-8 text-center">
              <h2 className="text-2xl font-bold mb-4">Ready to decode your legacy COBOL?</h2>
              <p className="text-muted-foreground mb-6">
                Start transforming complex COBOL code into clear documentation today.
              </p>
              <div className="flex justify-center gap-4">
                <Link to="/upload">
                  <Button size="lg" className="gap-1">
                    Try it now
                    <MoveRight className="h-4 w-4" />
                  </Button>
                </Link>
                <Link to="/documentation">
                  <Button variant="outline" size="lg">
                    Learn more
                  </Button>
                </Link>
              </div>
            </div>
          </div>
        </div>
      </main>
      
      <Footer />
    </div>
  );
};

export default About;
