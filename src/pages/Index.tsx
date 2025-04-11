
import React from 'react';
import Header from '@/components/Header';
import Footer from '@/components/Footer';
import HeroSection from '@/components/HeroSection';
import FeaturesSection from '@/components/FeaturesSection';
import ProcessSection from '@/components/ProcessSection';
import { Button } from '@/components/ui/button';
import { Link } from 'react-router-dom';
import { Terminal, ArrowRight, Github } from 'lucide-react';

const Index = () => {
  return (
    <div className="min-h-screen flex flex-col">
      <Header />
      
      <main className="flex-grow">
        <HeroSection />
        <FeaturesSection />
        <ProcessSection />
        
        {/* CTA Section */}
        <section className="py-16 bg-cobol-blue text-white">
          <div className="container">
            <div className="max-w-3xl mx-auto text-center">
              <h2 className="text-3xl font-bold mb-4">Ready to unlock your legacy code?</h2>
              <p className="text-lg mb-8 text-blue-100">
                Try COBOL Code Whisperer today and transform your COBOL codebase into clear, modern documentation.
              </p>
              <div className="flex gap-4 justify-center">
                <Link to="/upload">
                  <Button size="lg" variant="secondary" className="gap-1">
                    Get Started
                    <ArrowRight className="h-4 w-4" />
                  </Button>
                </Link>
                <Link to="/documentation">
                  <Button size="lg" variant="outline" className="bg-transparent border-white text-white hover:bg-white/10">
                    Learn More
                  </Button>
                </Link>
              </div>
            </div>
          </div>
        </section>
      </main>
      
      <Footer />
    </div>
  );
};

export default Index;
