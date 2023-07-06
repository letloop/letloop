#!chezscheme
(library (letloop html)
  (export
   html-read
   html-write
   ~check-letloop-html-write-0
   ~check-letloop-html-write-1
   ~check-letloop-html-write-2
   ~check-letloop-html-write-3
  )
  (import (chezscheme) (letloop match) (letloop html htmlprag))

  ;; ref: https://html.spec.whatwg.org/

  (define html-element-no-end-tag
    '(area
      base
      br
      col
      command
      embed
      hr
      img
      input
      keygen
      link
      meta
      param
      source
      track
      wbr))

  (define html-element-no-end-tag?
    (lambda (tag)
      (pair? (memq tag html-element-no-end-tag))))

  (define html-character->string
    (lambda (char)
      (cdr
       (or (assv char
                 '((#\" . "&quot;")
                   (#\& . "&amp;")
                   (#\< . "&lt;")
                   (#\> . "&gt;")))
           (cons char (list->string (list char)))))))

  (define string->html-string
    (lambda (string)
      (apply string-append
             (map html-character->string (string->list string)))))

  (define html-doctype "<!DOCTYPE html>")

  (define html-write-tag-start
    (lambda (tag attributes accumulator)
      (accumulator (format #f "<~a" tag))
      (for-each
       (lambda (attribute)
         (accumulator (format #f " ~a=\"~a\""
                              (car attribute)
                              (cadr attribute))))
       attributes)
      (if (html-element-no-end-tag? tag)
          (accumulator "/>")
          (accumulator ">"))))

  (define html-write-tag-end
    (lambda (tag accumulator)
      (unless (html-element-no-end-tag? tag)
        (accumulator (format #f "</~a>" tag)))))

  (define html-write
    (lambda (object accumulator)
      (cond
       ((string? object) (accumulator (string->html-string object)))
       ((number? object) (accumulator (number->string object)))
       (else
        (match object
               ((,tag (@ ,attributes ...) ,elements ...)
                (html-write-tag-start tag attributes accumulator)
                (for-each (lambda (element) (html-write element accumulator))
                          elements)
                (html-write-tag-end tag accumulator))
               ((,tag ,elements ...)
                (html-write-tag-start tag '() accumulator)
                (for-each (lambda (element) (html-write element accumulator))
                          elements)
                (html-write-tag-end tag accumulator)))))))

  (define html-read html->sxml)

  (define make-accumulator
    (lambda ()
      (let ((out '()))
        (lambda (object)
          (if (eof-object? object)
              (apply string-append (reverse out))
              (set! out (cons object out)))))))

  (define ~check-letloop-html-write-0
    (lambda ()
      (let ((accumulator (make-accumulator))
            (html `(h1 "hello" (b "world"))))
        (html-write html accumulator)

        (assert
         (string=? "<h1>hello<b>world</b></h1>"
                   (accumulator (eof-object)))))))

  (define ~check-letloop-html-write-1
    (lambda ()
      (let ((accumulator (make-accumulator))
            (html `(h1 "<&>!")))
        (html-write html accumulator)

        (assert
         (string=? "<h1>&lt;&amp;&gt;!</h1>"
                   (accumulator (eof-object)))))))

  (define ~check-letloop-html-write-2
    (lambda ()
      (let ((accumulator (make-accumulator))
            (html `(a (@ (href "https://hyper.dev"))
                      "hello you")))
        (html-write html accumulator)
        (assert
         (string=? "<a href=\"https://hyper.dev\">hello you</a>"
                   (accumulator (eof-object)))))))

  (define ~check-letloop-html-write-3
    (lambda ()
      (let ((accumulator (make-accumulator))
            (html `(p "echo" (br) "bravo")))
        (html-write html accumulator)
        (assert
         (string=? "<p>echo<br/>bravo</p>"
                   (accumulator (eof-object)))))))

  )
