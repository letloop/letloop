#!chezscheme
(import (chezscheme))
(import (letloop cli base))


(when (null? (cdr (command-line)))
  (letloop-usage)
  (exit 0))

(case (cadr (command-line))
  (("check") (letloop-check (cddr (command-line))))
  (("compile") (letloop-compile (cddr (command-line))))
  (("exec") (letloop-exec (cddr (command-line))))
  (("repl") (letloop-repl (cddr (command-line))))
  ;; (("html" "write" ,file) (letloop-html-write file))
  ;; (("html" "read" ,file) (letloop-html-read file))
  ;; (("html" "bootstrap" ,file) (letloop-html-bootstrap file))
  (else (letloop-usage) (exit 1)))
