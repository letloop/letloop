Usage:

  letloop check [--fail-fast] DIRECTORY-or-LIBRARY ...
  letloop compile PROGRAM.SCM
  letloop exec PROGRAM.SCM [ -- ARGUMENT ...]
  letloop repl

All subcommands can use directories, and extension of libraries as
standalone arguments.

The following flags are available:

  --dev

  Generate allocation, and instruction counts, debug on exception, and
  dump profile information.

  --optimize-level=0-3

  Configure optimization level, higher is less safe, but faster
