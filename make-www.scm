(import (chezscheme))

(define pk
  (lambda args
    (display ";; ")
    (write args)
    (newline)
    (car (reverse args))))

(define ftw
  (lambda (directory)
    (let loop ((paths (map (lambda (x) (string-append directory "/" x)) (directory-list directory)))
               (out '()))
      (if (null? paths)
          out
          (if (file-directory? (car paths))
              (loop (append (ftw (car paths)) (cdr paths))
                    out)
              (loop (cdr paths) (cons (car paths) out)))))))

(define directories (filter (lambda (x) (and (file-directory? x)
                                             (not (char=? (string-ref x 0) #\.))
                                             (not (string=? x "local"))))
                            (directory-list (current-directory))))

(define md?
  (lambda (name)
    (and (not (file-directory? name))
         (let ((length (string-length name)))
           (and (char=? #\d (string-ref name (- length 1)))
                (char=? #\m (string-ref name (- length 2)))
                (char=? #\. (string-ref name (- length 3))))))))

(define pandoc-html-command
  (lambda (md)

    (define strip-md
      (lambda (name)
        (substring name 0 (- (string-length name) 3))))

    (format #f "pandoc --from=markdown+yaml_metadata_block+inline_notes --mathml --standalone ~a/~a --template=~a/template.html --output=~a/~a.html"
            (current-directory) md (current-directory) (current-directory) (strip-md md))))

(define filenames (filter md? (apply append (map ftw directories))))

(for-each system (map pandoc-html-command filenames))

(system (pandoc-html-command "index.md"))
(system (pandoc-html-command "about/index.md"))
