#!chezscheme
(import (chezscheme))
(import (letloop cli base))
(import (letloop root))
(import (letloop literally))


(when (null? (cdr (command-line)))
  (letloop-usage)
  (exit 0))

(case (string->symbol (cadr (command-line)))
  ((benchmark) (letloop-benchmark (cddr (command-line))))
  ((check) (letloop-check (cddr (command-line))))
  ((compile) (letloop-compile (cddr (command-line))))
  ((exec) (letloop-exec (cddr (command-line))))
  ((literally) (letloop-literally (caddr (command-line))))
  ((repl) (letloop-repl (cddr (command-line))))
  ((root) (letloop-root (cddr (command-line))))
  (else (letloop-usage) (exit 1)))
