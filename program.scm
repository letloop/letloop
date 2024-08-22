(import (chezscheme))
(import (letloop cli compile)) 
(letloop-compile (list "library" "library/letloop/cli/base.scm" "letloop-main"))
