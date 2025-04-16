
import React from 'react';
import { Alert, AlertTitle, AlertDescription } from "@/components/ui/alert";
import { AlertCircle } from 'lucide-react';
import { ModelStatus as ModelStatusType } from '@/services/api';

interface ModelStatusProps {
  modelStatus: ModelStatusType | null;
  modelChecked: boolean;
}

const ModelStatusIndicator: React.FC<ModelStatusProps> = ({ modelStatus, modelChecked }) => {
  if (!modelStatus || !modelChecked) return null;

  return modelStatus.status === "ready" ? (
    <div className="flex items-center text-sm text-emerald-600 dark:text-emerald-400">
      <span className="h-2 w-2 rounded-full bg-emerald-500 mr-2"></span>
      LLaMA 3 model loaded and ready
    </div>
  ) : (
    <Alert variant="destructive" className="mt-2">
      <AlertCircle className="h-4 w-4" />
      <AlertTitle>LLaMA 3 model not loaded</AlertTitle>
      <AlertDescription>
        The LLM is not available. Analysis will use fallback methods, which may provide less accurate results.
      </AlertDescription>
    </Alert>
  );
};

export default ModelStatusIndicator;
