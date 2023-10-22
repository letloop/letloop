(library (letloop seed)
  (export seed-load seed-eval make-seed-environment)
  (import (chezscheme) (letloop r999) (letloop match))
  ;; TODO: use env's eval everywhere

  (define pk
    (lambda args
      (display ";; ")
      (write args)
      (newline)
      (flush-output-port)
      (car (reverse args))))

  (define-record-type* <operative>
    (make-operative~ meta source env)
    object-operative?
    (meta operative-meta)
    (source operative-source)
    (env operative-env))

  (define make-operative
    (case-lambda
     ((meta) (make-operative~ meta #f #f))
     ((meta source env) (make-operative~ meta source env))))
  
  (define-record-type* <environment>
    (make-environment~ env)
    object-environment?~
    (env environment-env*))

  (define environment-env
    (lambda (who x)
      (environment-env* x)))

  (define-record-type* <applicative>
    (make-applicative~ operative)
    object-applicative?
    (operative unwrap))

  (define combiner?
    (lambda (x)
      (or (object-applicative? x) (object-operative? x))))

  (define make-applicative
    (case-lambda
     ((meta) (make-applicative~ (make-operative meta)))
     ((meta source env) (make-applicative~ (make-operative meta source env)))))

  (define object-environment-ref
    (lambda (env symbol)
      (let loopx ((frames (environment-env 'ref env)))
        (when (null? frames)
          (error 'object "symbol not found" symbol))
        (let loopy ((frame (unbox (car frames))))
          (if (null? frame)
              (loopx (cdr frames))
              (if (eq? (caar frame) symbol)
                  (cdar frame)
                  (loopy (cdr frame))))))))

  (define meta-apply
    (lambda (name combiner args env)
      (cond
       ((object-operative? combiner)
        ((operative-meta combiner) env args))
       ((object-applicative? combiner)
        (let ((proc (operative-meta (unwrap combiner))))
          (proc env
                (map (lambda (x) (meta-eval x env)) args))))
       (else (error 'object "combiner not found" name)))))

  (define (meta-eval exp env)
    (cond
     ((symbol? exp) (object-environment-ref env exp))
     ((number? exp) exp)
     ((boolean? exp) exp)
     ((string? exp) exp)
     ((pair? exp)
      (meta-apply (car exp)
                  (meta-eval (car exp) env)
                  (cdr exp)
                  env))
     (else exp)))

  (define object-environment-set!
    (lambda (env symbol object)
      (define frame (car (environment-env 'set env)))

      (define set!
        (lambda (alist key value)
          (define item (assq key alist))
          (if (not (pair? item))
              (error 'object "Can not set variable that is not defined in scope" symbol)
              (and (set-cdr! item value)
                   alist))))

      (set-box! frame (set! (unbox frame) symbol object))))

  (define object-environment-define!
    (lambda (env symbol object)
      (define frame (car (environment-env 'define env)))
      (set-box! frame (cons (cons symbol object) (unbox frame)))))

  (define (make-environment alist)
    (make-environment~ (list (box (list)) (box alist))))

  (define (environment-cons env alist)
    (make-environment~ (cons (box alist) (environment-env 'cons env))))

  (define (environment-cons* env env*)
    (make-environment~ (cons (environment-env 'cons* env*)
                             (environment-env 'cons* env))))

  (define object-ground
    (box (list)))

  (define make-object-ground!
    (lambda (key value)
      (define set!
        (lambda (alist key value)
          (define item (assq key alist))
          (if (not (pair? item))
              (cons (cons key value) alist)
              (and (set-cdr! item value)
                   alist))))

      (set-box! object-ground
                (set! (unbox object-ground) key value))
      value))

  (define object-sum
    (make-object-ground! 'sum
      (make-applicative
       (lambda (env args)
         (define combiner (car args))
         (assert (combiner? combiner))
         (if (object-applicative? combiner)
             (object-sum (unwrap combiner))
             (cons (operative-source combiner)
                   (operative-env combiner)))))))                          

  (define object-environment-cons*
    (make-object-ground! 'environment-cons*
      (make-applicative (lambda (env args) (apply environment-cons* args)))))

  (define object-environment-cons
    (make-object-ground! 'environment-cons
      (make-applicative (lambda (env args) (apply environment-cons args)))))

  (define object-make-environment
    (make-object-ground! 'make-environment
      (make-applicative
       (lambda (env args)
         (match args
           ((,env ((,a* . ,b*) ...))
            (environment-cons env (map cons a* b*))))))))

  (define object-pk
    (make-object-ground! 'pk
      (make-applicative (lambda (env args) (apply pk args)))))

  (define object-environment?
    (make-object-ground! 'environment?
      (make-applicative (lambda (env args) (apply object-environment?~ args)))))

  (define object-list
    (make-object-ground! 'list
      (make-applicative (lambda (env args)  args))))

  (define object-map
    (make-object-ground! 'map
      (make-applicative
       (lambda (env args)
         (define proc (operative-meta (unwrap (car args))))
         (apply map
                (lambda x
                  (proc env x))
                (cdr args))))))

  (define object-list*
    (make-object-ground! 'list*
      (make-applicative (lambda (env args)
                          (apply cons* args)))))

  (define unwrap*
    (lambda (x)
      (if (object-operative? x)
          (operative-meta x)
          (operative-meta (unwrap x)))))

  (define object-apply
    (make-object-ground! 'apply
      (make-applicative
       (lambda (env args)
         (match args
           ((,proc ,env ,args)
            (apply (unwrap* proc) env (list args))))))))

  (define object-error
    (make-object-ground! 'error
      (make-applicative
       (lambda (env args)
         (match args
           ((,who ,message . ,args)
            (error who message args)))))))

  (define arguments-with-rest?
    (lambda (args)
      (let loop ((args args)
                 (positionals '()))
        (if (null? args)
            (values #f #f #f)
            (if (not (pair? (cdr args)))
                (values #t
                        (reverse (cons (car args) positionals))
                        (cdr args))
                (loop (cdr args)
                      (cons (car args) positionals)))))))

  (define parse-arguments-with-rest
    (lambda (positionals rest args)
      (let loop ((positionals positionals)
                 (args args)
                 (out '()))

        (cond
         ((and (null? positionals) (null? args))
          (reverse (cons (cons rest '()) out)))
         ((and (not (null? positionals))
               (null? args))
          (error 'object "wrong number of arguments"))
         ((null? positionals)
          (reverse (cons (cons rest args) out)))
         (else (loop (cdr positionals) (cdr args)
                     (cons (cons (car positionals)
                                 (car args))
                           out)))))))

  (define object-vau
    (make-object-ground! 'vau
      (make-operative
       (lambda (static-env args)
         (define source args)
         (match (cdr args)
           ((,dynamic-env-name ,body ...)
            (if (symbol? (car args))
                (match args
                  ((,formal ,dynamic-env-name ,body ...)
                   (make-operative
                    (lambda (dynamic-env args)
                      (let* ((alist (list (cons formal args)
                                          (cons dynamic-env-name dynamic-env)))
                             (env* (environment-cons static-env alist)))
                        (meta-eval `(sequence ,@body) env*)))
                    source
                    static-env)))
                (call-with-values (lambda () (arguments-with-rest? (car args)))
                  (lambda (rest? positionals rest)
                    (if rest?
                        (make-operative
                         (lambda (dynamic-env args*)
                           (let* ((alist (cons (cons dynamic-env-name dynamic-env)
                                               (parse-arguments-with-rest positionals rest args*)))
                                  (env* (environment-cons static-env alist)))
                             (meta-eval `(sequence ,@body) env*)))
                         source
                         static-env)
                        (make-operative
                         (lambda (dynamic-env args*)
                           (let* ((alist (map cons args args*))
                                  (env* (environment-cons static-env alist)))
                             (meta-eval `(sequence ,@body) env*)))
                         source
                         static-env)))))))))))

  (define object-lambda
    (make-object-ground! 'lambda
      (make-operative
       (lambda (_ args)
         (match args
           (((,formals ...) ,body ...)
            (make-applicative
             (lambda (env args)
               (let* ((alist (map cons formals args))
                      (env* (environment-cons env alist)))
                 (meta-eval `(sequence ,@body) env*))))))))))

  (define object-eval
    (make-object-ground! 'eval
      (make-applicative (lambda (env args) (apply meta-eval args)))))

  (define object-sequence
    (make-object-ground! 'sequence
      (make-operative (lambda (env args)
                        (let loop ((args args))
                          (let ((out (meta-eval (car args) env)))
                            (if (null? (cdr args))
                                out
                                (loop (cdr args)))))))))

  (define object-+
    (make-object-ground! '+
      (make-applicative
       (lambda (env args)
         (apply + args)))))

  (define object--
    (make-object-ground! '-
      (make-applicative
       (lambda (env args)
         (apply - args)))))

  (define object-=
    (make-object-ground! '=
      (make-applicative
       (lambda (env args)
         (apply = args)))))

  (define object-car
    (make-object-ground! 'car
      (make-applicative
       (lambda (env args)
         (caar args)))))

  (define object-cdr
    (make-object-ground! 'cdr
      (make-applicative
       (lambda (env args)
         (cdar args)))))

  (define object-define
    (make-object-ground! 'define
      (make-operative
       (lambda (env args)
         (match args
           ((,d ,o)
            (object-environment-define! env
                                        d
                                        (meta-eval o env)))
           ((,target ,d ,o)
            (object-environment-define! target
                                        d
                                        (meta-eval o env))))))))

  (define object-if
    (make-object-ground! 'if
      (make-operative
       (lambda (env args)
         (match args
           ((,a ,b ,c)

            (if (eq? (meta-eval a env) #f)
                (begin
                  (meta-eval c env))
                (begin
                  (meta-eval b env)))))))))

  (define object-cons
    (make-object-ground! 'cons
      (make-applicative
       (lambda (env args)
         (apply cons args)))))

  (define object-void
    (make-object-ground! 'void
      (make-applicative
       (lambda (env args)
         (void)))))

  (define object-print
    (make-object-ground! 'print
      (make-applicative
       (lambda (env args)
         (display (car args))
         (newline)
         (flush-output-port)))))
 
  (define object-exit
    (make-object-ground! 'exit
      (make-applicative
       (lambda (env args)
         (if (car args)
             (exit 0)
             (exit 1))))))

  (define object-null?
    (make-object-ground! 'null?
      (make-applicative
       (lambda (env args)
         (null? (car args))))))

  (define object-not
    (make-object-ground! 'not
      (make-applicative
       (lambda (env args)
         (not (car args))))))

  (define object-symbol?
    (make-object-ground! 'symbol?
      (make-applicative
       (lambda (env args)
         (symbol? (car args))))))

  (define object-string=?
    (make-object-ground! 'string=?
      (make-applicative
       (lambda (env args)
         (apply string=? args)))))

  (define object-eq?
    (make-object-ground! 'eq?
      (make-applicative
       (lambda (env args)
         (apply eq? args)))))

  (define object-alien
    (make-object-ground! 'alien
      (make-operative
       (lambda (env args)
         (define library #f)
         (define proc #f)
         (if (= (length args) 1)
             (begin
               (set! library '(chezscheme))
               (set! proc (car args)))
             (begin
               (set! library (car args))
               (set! proc (cadr args))))
         (let ((proc (eval proc (environment library))))
           (make-applicative
            (lambda (env args) (apply proc args))))))))
  
  (define (make-seed-environment)
    (make-environment~ (list (box (list)) object-ground)))

  (define seed-load
    (lambda (filenames)

      (define ground (make-seed-environment))

      (let loop ((filenames filenames))
        (unless (null? filenames)
          (call-with-input-file (car filenames)
            (lambda (p)
              (define exp (lambda () (read p)))
              (let loop ()
                (let ((out (exp)))
                  (unless (eof-object? out)
                    (meta-eval out ground)
                    (loop))))))
          (loop (cdr filenames))))))

  (define seed-eval
    (case-lambda
     ((exp) (meta-eval exp (make-seed-environment)))
     ((exp env) (meta-eval exp env))))
  
  )
