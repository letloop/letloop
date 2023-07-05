(import (chezscheme))
(import (letloop blake3))


(define port (open-file-input-port (cadr (command-line))))
(format #t "~{~x~}\n" (bytevector->u8-list (blake3 (get-bytevector-all port))))
(flush-output-port)
(close-port port)
