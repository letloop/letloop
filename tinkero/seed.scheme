(define echo
  (lambda (object)
    (define port (open-output-string))
    (write object port)
    (let ((out (get-output-string port)))
      (close-port port)
      out)))

(define pk
  (lambda args
    (console-log (echo args))
    (car (reverse args))))

(define pke
  (lambda args
    (console-error (echo args))
    (car (reverse args))))

(define error
  (lambda (who message . what)
    (apply pke who message what)
    (raise 'error)))

(define exp-add
  (lambda (exp path object)
    (when (null? path)
      (error 'exp-add "invalid path 0" path))
    (let ((target (car path)))
      (let loop ((out '())
                 (index 0)
                 (exp exp))
        (if (= index target)
            (if (null? (cdr path))
                (append (reverse out)
                        (list object)
                        exp)
                (if (pair? (car exp))
                    (append (reverse out)
                            (list (exp-add (car exp) (cdr path) object))
                            (cdr exp))
                    (error 'exp-add "invalid path 1" path)))
            (loop (cons (car exp) out)
                  (+ index 1)
                  (cdr exp)))))))

(define exp-remove
  (lambda (exp path)
    (pk 'exp exp)
    (when (null? path)
      (error 'exp-add "invalid path 0" path))
    (let ((target (car path)))
      (let loop ((out '())
                 (index 0)
                 (exp exp))
        (pk 'index index)
        (if (= index target)
            (if (null? (cdr path))
                (append (reverse out)
                        (cdr exp))
                (if (pair? (car exp))
                    (append (reverse out)
                            (list (exp-remove (car exp) (cdr path)))
                            (cdr exp))
                    (error 'exp-add "invalid path 1" path)))
            (loop (cons (car exp) out)
                  (+ index 1)
                  (cdr exp)))))))

(define exp-replace
  (lambda (exp path object)
    (exp-remove
     (exp-add exp path object)
     (let ((rpath (reverse path)))
       (reverse (cons (+ 1 (car rpath))
                      (cdr rpath)))))))

;; (echo (exp-replace '(lambda (a) (arnu 40 2 a)) '(2 2) 1))

(define exp-html-base
  (lambda (exp)
    (if (pair? exp)
        `(div.parents
          #;(div.editor
           (a href "" "🌱")
           (a href "" "↕️")
           (a href "" "🗑️"))
          ,@(let ((head (car exp)))
              (list (exp-html-base head)
                    `(div.operands
                      ,@(map exp-html-base (cdr exp))))))
        (if (symbol? exp)
            ;; TODO: add string
            `(div.datum (div.symbol ,(symbol->string exp)))
            `(div.datum (div.symbol ,(number->string exp)))))))

(define exp-html
  (lambda (exp)
    (element-new
     `(div.line
       ,(exp-html-base exp)
       (div.editor
        (a href "" "kdem 🚀"))))))

(define root (getelem "#root"))

(define other (exp-html '(define exp-html
                           (lambda (exp)
                             (element-new
                              `(div.line
                                ,(exp-html-base exp)))))))


(element-insert! root other)


(element-insert! root
                 (element-new
                  `(div.line (div.editor (a href "" "define")))))
