
import { toast } from "sonner";

const API_URL = "http://localhost:8000";

export interface AnalysisResult {
  file_id: string;
  summary: string;
  business_rules: string[];
  code_structure: {
    division: string;
    elements: {
      name: string;
      description: string;
    }[];
  }[];
  complexity: {
    cyclomatic: number;
    linesOfCode: number;
    commentPercentage: number;
    dataItems?: number;
    paragraphs?: number;
    ifStatements?: number;
    performStatements?: number;
  };
  call_graph?: Record<string, string[]>;
  data_flow?: Record<string, Array<{variable: string, operation: string}>>;
  chunks?: Array<{
    type: string;
    name: string;
    content: string;
    start_line: number;
    end_line: number;
    line_count: number;
  }>;
}

export interface FeedbackData {
  file_id: string;
  chunk_id?: string;
  rating: number; // -1 for negative, 0 for neutral, 1 for positive
  comment?: string;
  corrected_summary?: string;
}

export interface ModelStatus {
  status: "ready" | "not_loaded";
  model_info?: {
    name?: string;
    type?: string;
    parameters?: number;
    loaded_at?: string;
  };
}

// Check if the LLM model is loaded and ready
export const checkModelStatus = async (): Promise<ModelStatus> => {
  try {
    console.log("Checking model status at:", `${API_URL}/api/model-status`);
    const response = await fetch(`${API_URL}/api/model-status`);
    
    if (!response.ok) {
      console.error("Failed to check model status:", response.status, response.statusText);
      throw new Error(`Failed to check model status: ${response.status}`);
    }
    
    return await response.json();
  } catch (error) {
    console.error("Error checking model status:", error);
    return { status: "not_loaded" };
  }
};

// Function to upload and analyze a COBOL file
export const analyzeCobolFile = async (file: File): Promise<AnalysisResult> => {
  try {
    const formData = new FormData();
    formData.append("file", file);
    
    console.log("Sending file analysis request to:", `${API_URL}/api/analyze-code/`);

    const response = await fetch(`${API_URL}/api/analyze-code/`, {
      method: "POST",
      body: formData,
    });

    if (!response.ok) {
      const errorData = await response.json();
      console.error("Analysis failed:", response.status, errorData);
      throw new Error(errorData.detail || "Failed to analyze file");
    }

    return await response.json();
  } catch (error) {
    console.error("Error analyzing file:", error);
    toast.error("Failed to analyze COBOL file", {
      description: error instanceof Error ? error.message : "Unknown error occurred",
    });
    throw error;
  }
};

// Function to submit feedback on analysis
export const submitFeedback = async (feedback: FeedbackData): Promise<void> => {
  try {
    const response = await fetch(`${API_URL}/api/feedback/`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(feedback),
    });

    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.detail || "Failed to submit feedback");
    }

    toast.success("Feedback submitted successfully", {
      description: "Thank you for helping improve our AI model!",
    });
  } catch (error) {
    console.error("Error submitting feedback:", error);
    toast.error("Failed to submit feedback", {
      description: error instanceof Error ? error.message : "Unknown error occurred",
    });
    throw error;
  }
};

// Function to get all feedback
export const getAllFeedback = async (): Promise<FeedbackData[]> => {
  try {
    const response = await fetch(`${API_URL}/api/feedback/`);

    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.detail || "Failed to retrieve feedback");
    }

    const data = await response.json();
    return data.feedback;
  } catch (error) {
    console.error("Error retrieving feedback:", error);
    toast.error("Failed to retrieve feedback", {
      description: error instanceof Error ? error.message : "Unknown error occurred",
    });
    throw error;
  }
};

// Function to get file content by ID
export const getFileContent = async (fileId: string): Promise<{ filename: string; content: string; lines: number }> => {
  try {
    const response = await fetch(`${API_URL}/api/file/${fileId}`);

    if (!response.ok) {
      const errorData = await response.json();
      throw new Error(errorData.detail || "Failed to retrieve file");
    }

    return await response.json();
  } catch (error) {
    console.error("Error retrieving file:", error);
    toast.error("Failed to retrieve file content", {
      description: error instanceof Error ? error.message : "Unknown error occurred",
    });
    throw error;
  }
};
