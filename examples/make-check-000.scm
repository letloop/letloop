(library (make-check-000)
  (export main)
  (import (chezscheme) (letloop json))

  (define main
    (lambda ()
      (pretty-print (vector-ref (json-read) 70)))))
