
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
  };
}

export interface FeedbackData {
  file_id: string;
  rating: number;
  comment?: string;
  corrected_summary?: string;
}

// Function to upload and analyze a COBOL file
export const analyzeCobolFile = async (file: File): Promise<AnalysisResult> => {
  try {
    const formData = new FormData();
    formData.append("file", file);

    const response = await fetch(`${API_URL}/analyze-code/`, {
      method: "POST",
      body: formData,
    });

    if (!response.ok) {
      const errorData = await response.json();
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
    const response = await fetch(`${API_URL}/feedback/`, {
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

// Function to get file content by ID
export const getFileContent = async (fileId: string): Promise<{ filename: string; content: string; lines: number }> => {
  try {
    const response = await fetch(`${API_URL}/file/${fileId}`);

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
