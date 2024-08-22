(import (only (chezscheme) import command-line-arguments apply))
(import (letloop cli base))

(apply letloop-main (command-line-arguments))
