Usage:

  letloop benchmark LIBRARY [THUNK [N]]
  letloop check [--fail-fast] LIBRARY ...
  letloop compile LIBRARY.SCM PROCEDURE
  letloop exec PROGRAM.SCM [ -- ARGUMENT ...]
  letloop repl
  letloop root available
  letloop root init DISTRIBUTION VERSION MACHINE DIRECTORY
  letloop root chroot DIRECTORY -- COMMAND ... ARGS
  letloop root exec DIRECTORY TARGET-DIRECTORY -- COMMAND ARGS ...
  letloop root spawn DIRECTORY
  letloop root emulate DIRECTORY
  
The following flags are available:

  --dev Generate allocation, and instruction counts, debug on
        exception, and dump profile information.

  --optimize-level=0-3 Configure optimization level, higher is less
                       safe, but faster
