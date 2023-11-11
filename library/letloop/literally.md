# `(import (letloop literally))`

## `(literally port)`

Translate markdown into a scheme library.

Example markdown file:

~~~markdown
# Library `(my-markdown-library)`

```scheme
(import (chezscheme))
```

## Procedure `(foo bar qux)`

```scheme
(define foo (lambda (bar qux) 42))

(define ~check-my-markdown-library (lambda () #t))

(define ~benchmark-my-markdown-library (lambda () (bar 1337)))
```
~~~

Is translated into:

```scheme
(library (my-markdown-library)
  (export foo ~check-my-markdown-library ~benchmark-my-markdown-library)
  (import (chezscheme))

  (define foo (lambda (bar qux) 42))
  (define ~check-my-markdown-library (lambda () #t))
  (define ~benchmark-my-markdown-library (lambda () (bar 1337))))
```
