(library (letloop commonmark)
  (export commonmark-read ~check-commonmark-000)
  (import (chezscheme) (letloop html))


  (define llibcmark (load-shared-object "libcmark.so"))

  (define commonmark-read
      ;; binding for: char *cmark_markdown_to_html(const char *text, size_t len, int options);
    (let ((cmark_markdown_to_html (foreign-procedure "cmark_markdown_to_html"
                                                     (string size_t int) string)))
      (lambda (string)
        (cmark_markdown_to_html string (bytevector-length (string->utf8 string)) 0))))

  (define pk
    (lambda args
      (write args)
      (newline)
      (car (reverse args))))

  (define ~check-commonmark-000
    (lambda ()
      (equal? '(*TOP* (p "echo " (strong "alpha") " bravo") "\n")
              (pk (html-read (commonmark-read "echo **alpha** bravo")))))))
