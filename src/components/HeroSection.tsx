
import React from 'react';
import { Terminal, ArrowRight } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Link } from 'react-router-dom';

const HeroSection = () => {
  return (
    <div className="relative overflow-hidden before:absolute before:top-0 before:start-1/2 before:bg-[url('https://preline.co/assets/svg/examples/polygon-bg-element.svg')] before:bg-no-repeat before:bg-top before:bg-cover before:w-full before:h-full before:-z-[1] before:transform before:-translate-x-1/2">
      <div className="max-w-[85rem] mx-auto px-4 sm:px-6 lg:px-8 pt-24 pb-10">
        <div className="flex justify-center">
          <div className="inline-flex items-center gap-2 px-3 py-2 rounded-full bg-primary/10 text-primary text-sm">
            <span className="font-semibold">Unveiling Legacy Code</span>
            <span className="shrink-0 w-2 h-2 bg-primary rounded-full"></span>
            <span>Local AI-Powered</span>
          </div>
        </div>

        <div className="mt-5 max-w-3xl text-center mx-auto">
          <h1 className="block font-bold text-gray-800 text-4xl md:text-5xl lg:text-6xl dark:text-gray-200">
            <span className="bg-clip-text bg-gradient-to-r from-cobol-blue to-primary text-transparent">COBOL Code</span> Whisperer
          </h1>
        </div>

        <div className="mt-5 max-w-2xl text-center mx-auto">
          <p className="text-lg text-gray-600 dark:text-gray-400">
            Documentation and business logic extraction for COBOL systems using local LLaMA 3 with continuous human feedback fine-tuning.
          </p>
        </div>

        <div className="mt-8 gap-3 flex justify-center">
          <Link to="/upload">
            <Button size="lg" className="gap-1">
              Try it now
              <ArrowRight className="h-4 w-4" />
            </Button>
          </Link>
          <Link to="/documentation">
            <Button variant="outline" size="lg">
              Learn more
            </Button>
          </Link>
        </div>
      </div>
      
      <div className="mx-auto max-w-5xl px-4 sm:px-6 lg:px-8 relative">
        <div className="relative bg-white rounded-xl overflow-hidden shadow-lg dark:bg-slate-900 border">
          <div className="bg-cobol-blue dark:bg-slate-800 px-4 py-3 flex items-center rounded-t-xl">
            <div className="flex space-x-1.5">
              <div className="h-3 w-3 bg-red-500 rounded-full"></div>
              <div className="h-3 w-3 bg-yellow-500 rounded-full"></div>
              <div className="h-3 w-3 bg-green-500 rounded-full"></div>
            </div>
            <div className="flex-1 flex justify-center">
              <div className="text-xs text-white font-medium">COBOL Analyzer</div>
            </div>
          </div>
          <div className="px-6 py-5 bg-gradient-to-b from-white to-slate-50 dark:from-slate-900 dark:to-slate-800 font-mono text-sm text-left overflow-x-auto whitespace-pre">
            <pre className="text-xs sm:text-sm text-gray-800 dark:text-gray-200 leading-relaxed">
<span className="text-cobol-blue">IDENTIFICATION DIVISION.</span>
<span className="text-cobol-blue">PROGRAM-ID.</span> <span className="text-cobol-gray">ACCTRANS.</span>

<span className="text-cobol-blue">DATA DIVISION.</span>
<span className="text-cobol-blue">WORKING-STORAGE SECTION.</span>
01 WS-TRANSACTION-RECORD.
   05 WS-ACCOUNT-NO      PIC X(10).
   05 WS-TRANS-TYPE      PIC X(1).
      88 WS-DEPOSIT      VALUE 'D'.
      88 WS-WITHDRAWAL   VALUE 'W'.
   05 WS-TRANS-AMOUNT    PIC 9(7)V99.

<span className="text-cobol-blue">PROCEDURE DIVISION.</span>
MAIN-LOGIC.
    <span className="text-cobol-green">* Process account transactions</span>
    PERFORM PROCESS-TRANSACTION
    DISPLAY "Transaction processed successfully"
    STOP RUN.

PROCESS-TRANSACTION.
    IF WS-DEPOSIT
       PERFORM HANDLE-DEPOSIT
    ELSE IF WS-WITHDRAWAL
       PERFORM HANDLE-WITHDRAWAL
    ELSE
       DISPLAY "Invalid transaction type"
    END-IF.
            </pre>
            
            <div className="flex items-center gap-3 mt-6 bg-emerald-50 p-4 rounded-md dark:bg-emerald-900/20">
              <Terminal className="h-5 w-5 text-emerald-500" />
              <span className="text-sm text-emerald-700 dark:text-emerald-400 animate-typing overflow-hidden whitespace-nowrap">
                Analyzing business logic...
              </span>
              <div className="flex">
                <span className="processing-dot text-lg text-emerald-500">.</span>
                <span className="processing-dot text-lg text-emerald-500">.</span>
                <span className="processing-dot text-lg text-emerald-500">.</span>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default HeroSection;
