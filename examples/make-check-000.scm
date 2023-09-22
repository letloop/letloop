(import (chezscheme))
(import (letloop json))


(pretty-print (vector-ref (json-read) 70))
