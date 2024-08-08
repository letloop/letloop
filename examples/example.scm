(import (chezscheme))

(display "Expected output is what remains toward a super star number: ")
(display (- (+ 2600 1337) (string->number (cadr (command-line)))))
(newline)
(flush-output-port)
