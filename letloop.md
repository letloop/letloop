Usage:

  letloop check [--fail-fast] DIRECTORY-or-LIBRARY ...
  letloop compile PROGRAM.SCM A.OUT
  letloop exec PROGRAM.SCM [ -- ARGUMENT ...]
  letloop repl 
  
All subcommands directory, and extension of libraries as standalone
arguments.
  
Also the following flags are available:

  --dev  Generate allocation, and instruction counts, debug 
         on exception, dump profiling information.
  
  --optimize-level=0-3  Configure optimization level, 
                        higher is less safe, but faster.

Let there be loops!
