(scheme-start 
  (lambda args 
    (invoke-library '(letloop literally))
    (invoke-library '(letloop root))
    (invoke-library '(letloop cli base))
    (letloop-main)
    (display "echo\n")))
