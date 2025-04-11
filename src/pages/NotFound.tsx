
import React, { useEffect } from "react";
import { useLocation, Link } from "react-router-dom";
import { Button } from "@/components/ui/button";
import { AlertTriangle, Home, ArrowLeft } from "lucide-react";

const NotFound = () => {
  const location = useLocation();

  useEffect(() => {
    console.error(
      "404 Error: User attempted to access non-existent route:",
      location.pathname
    );
  }, [location.pathname]);

  return (
    <div className="min-h-screen flex items-center justify-center bg-background p-4">
      <div className="w-full max-w-md text-center">
        <div className="inline-flex items-center justify-center p-4 bg-yellow-100 dark:bg-yellow-900/20 rounded-full mb-6">
          <AlertTriangle className="h-10 w-10 text-yellow-600 dark:text-yellow-500" />
        </div>
        
        <h1 className="text-6xl font-bold text-cobol-blue">404</h1>
        <p className="mt-2 text-2xl font-semibold mb-6">Page Not Found</p>
        
        <p className="text-muted-foreground mb-8">
          The page you're looking for doesn't exist or has been moved.
        </p>
        
        <div className="flex flex-col sm:flex-row gap-4 justify-center">
          <Link to="/">
            <Button className="w-full sm:w-auto gap-2">
              <Home className="h-4 w-4" />
              Return Home
            </Button>
          </Link>
          
          <Button 
            variant="outline" 
            className="w-full sm:w-auto gap-2"
            onClick={() => window.history.back()}
          >
            <ArrowLeft className="h-4 w-4" />
            Go Back
          </Button>
        </div>
        
        <div className="mt-12 border-t pt-6">
          <p className="text-sm text-muted-foreground">
            If you believe this page should exist, please contact support.
          </p>
        </div>
      </div>
    </div>
  );
};

export default NotFound;
