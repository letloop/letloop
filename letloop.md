Usage:

  letloop check [--fail-fast] DIRECTORY ...
  letloop compile [DIRECTORY ...] PROGRAM.SCM A.OUT
  letloop exec [DIRECTORY ...] PROGRAM.SCM [ -- ARGUMENT ...]
  letloop repl [DIRECTORY ...]
  
You can pass file extensions of libraries as standalone arguments.
  
Also the following flags are available:

  --dev 
  
  Generate allocation, and instruction counts, debug on exception,
  dump profiling information.
  
  --optimize-level=0-3 
  
  Configure optimization level, higher is faster, and less safe.

Let there be loops!
