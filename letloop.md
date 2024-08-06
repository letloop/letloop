Usage:

  letloop benchmark LIBRARY [THUNK [N]]
  letloop check [--fail-fast] LIBRARY ...
  letloop compile PROGRAM.SCM
  letloop exec PROGRAM.SCM [ -- ARGUMENT ...]
  letloop literally LIBRARY.MD
  letloop repl
  letloop root available
  letloop root init DISTRIBUTION VERSION DIRECTORY
  letloop root chroot DIRECTORY -- COMMAND ... ARGS
  letloop root exec DIRECTORY TARGET-DIRECTORY -- COMMAND ARGS ...
  letloop root spawn DIRECTORY
  letloop root emulate DIRECTORY
  
The following flags are available:

  --dev Generate allocation, and instruction counts, debug on
        exception, and dump profile information.

  --optimize-level=0-3 Configure optimization level, higher is less
                       safe, but faster
