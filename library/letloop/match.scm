#!r6rs

;; Copyright (C) Marc Nieper-Wißkirchen (2022).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(library (letloop match)
  (export match
          unquote ... _ -> guard
          check~match-001
          check~match-002
          check~match-003
          check~match-004
          check~match-005
          check~match-006

          check~match-008
          check~match-009
          check~match-010
          check~match-011
          check~match-012
          check~match-013-0
          check~match-013-1
          check~match-014
          check~match-015
          check~match-016
          check~match-017
          check~match-018
          check~match-019
          check~match-020
          )
  (import (rnrs (6))
	  (letloop match helpers)
	  (letloop match quasiquote-transformer))

  (define-syntax ->
    (lambda (stx)
      (syntax-violation '-> "invalid use of auxiliary syntax" stx)))

  (define split
    (lambda (obj k succ fail)
      (let ([n (length+ obj)])
        (if (and n
                 (fx<=? k n))
            (call-with-values
                (lambda ()
                  (split-at obj (fx- n k)))
              succ)
            (fail)))))

  (define-syntax match
    (lambda (stx)
      (define who 'match)
      (define-record-type pattern-variable
        (nongenerative) (sealed #t) (opaque #t)
        (fields identifier expression level))
      (define-record-type cata-binding
        (nongenerative) (sealed #t) (opaque #t)
        (fields proc-expr value-id* identifier))
      (define make-identifier-hashtable
	(lambda ()
	  (define identifier-hash
	    (lambda (id)
	      (assert (identifier? id))
	      (symbol-hash (syntax->datum id))))
	  (make-hashtable identifier-hash bound-identifier=?)))
      (define check-pattern-variables
	(lambda (pvars)
	  (define ht (make-identifier-hashtable))
	  (for-each
	   (lambda (pvar)
	     (define id (pattern-variable-identifier pvar))
	     (hashtable-update! ht
				id
				(lambda (val)
				  (when val
				    (syntax-violation who "repeated pattern variable in match clause" stx id))
				  #t)
				#f))
	   pvars)))
      (define check-cata-bindings
	(lambda (catas)
	  (define ht (make-identifier-hashtable))
	  (for-each
	   (lambda (cata)
	     (for-each
	      (lambda (id)
		(hashtable-update! ht
				   id
				   (lambda (val)
				     (when val
				       (syntax-violation who "repeated cata variable in match clause" stx id))
				     #t)
				   #f))
	      (cata-binding-value-id* cata)))
	   catas)))
      (define parse-clause
        (lambda (cl)
          (syntax-case cl (guard)
            [(pat (guard guard-expr ...) e1 e2 ...)
             (values #'pat #'(and guard-expr ...) #'(e1 e2 ...))]
            [(pat e1 e2 ...)
             (values #'pat #'#t #'(e1 e2 ...))]
            [_
             (syntax-violation who "ill-formed match clause" stx cl)])))
      (define gen-matcher
        (lambda (expr pat)
          (define ill-formed-match-pattern-violation
            (lambda ()
              (syntax-violation who "ill-formed match pattern" stx pat)))
          (syntax-case pat (-> unquote)
            [,[f -> y ...]
             (for-all identifier? #'(y ...))
             (with-syntax ([(x) (generate-temporaries '(x))])
               (values
                (lambda (k)
                  (k))
                (list (make-pattern-variable #'x expr 0))
                (list (make-cata-binding #'f #'(y ...) #'x))))]
            [,[y ...]
             (for-all identifier? #'(y ...))
             (with-syntax ([(x) (generate-temporaries '(x))])
               (values
                (lambda (k)
                  (k))
                (list (make-pattern-variable #'x expr 0))
                (list (make-cata-binding #'loop #'(y ...) #'x))))]
            [(pat1 ell pat2 ... . pat3)
             (ellipsis? #'ell)
             (gen-ellipsis-matcher expr #'pat1 #'(pat2 ...) #'pat3)]
            [,x
             (identifier? #'x)
             (values
              (lambda (k)
                (k))
              (list (make-pattern-variable #'x expr 0))
              '())]
            [(pat1 . pat2)
             (with-syntax ([(e1 e2) (generate-temporaries '(e1 e2))])
               (let*-values ([(mat1 pvars1 catas1)
                              (gen-matcher #'e1 #'pat1)]
                             [(mat2 pvars2 catas2)
                              (gen-matcher #'e2 #'pat2)])
                 (values
                  (lambda (k)
                    #`(if (pair? #,expr)
                          (let ([e1 (car #,expr)]
                                [e2 (cdr #,expr)])
                            #,(mat1 (lambda () (mat2 k))))
                          (fail)))
                  (append pvars1 pvars2) (append catas1 catas2))))]
            [unquote
             (ill-formed-match-pattern-violation)]
            [_
             (values
              (lambda (k)
                #`(if (equal? #,expr '#,pat)
                      #,(k)
                      (fail)))
              '() '())])))
      (define gen-ellipsis-matcher
        (lambda (expr pat1 pat2* pat3)
          (with-syntax ([(e1 e2) (generate-temporaries '(e1 e2))])
            (let*-values ([(mat1 pvars1 catas1)
                           (gen-map #'e1 pat1)]
                          [(mat2 pvars2 catas2)
                           (gen-matcher* #'e2 (append pat2* pat3))])
              (values
               (lambda (k)
                 #`(split
                    #,expr
                    #,(length pat2*)
                    (lambda (e1 e2)
                      #,(mat1 (lambda () (mat2 k))))
                    fail))
               (append pvars1 pvars2)
               (append catas1 catas2))))))
      (define gen-matcher*
        (lambda (expr pat*)
          (syntax-case pat* (unquote)
            [()
             (values
              (lambda (k)
                #`(if (null? #,expr)
                      #,(k)
                      (fail)))
              '() '())]
            [,x
             (gen-matcher expr pat*)]
            [(pat . pat*)
             (with-syntax ([(e1 e2) (generate-temporaries '(e1 e2))])
               (let*-values ([(mat1 pvars1 catas1)
                              (gen-matcher #'e1 #'pat)]
                             [(mat2 pvars2 catas2)
                              (gen-matcher* #'e2 #'pat*)])
                 (values
                  (lambda (k)
                    #`(let ([e1 (car #,expr)]
                            [e2 (cdr #,expr)])
                        #,(mat1
                           (lambda ()
                             (mat2 k)))))
                  (append pvars1 pvars2)
                  (append catas1 catas2))))]
            [_
             (gen-matcher expr pat*)])))
      (define gen-map
        (lambda (expr pat)
          (with-syntax ([(e1 e2 f) (generate-temporaries '(e1 e2 f))])
            (let-values ([(mat ipvars catas)
                          (gen-matcher #'e1 pat)])
              (with-syntax ([(u ...)
                             (generate-temporaries ipvars)]
                            [(v ...)
                             (map pattern-variable-expression ipvars)])
                (values
                 (lambda (k)
                   #`(let f ([e2 (reverse #,expr)]
                             [u '()] ...)
                       (if (null? e2)
                           #,(k)
                           (let ([e1 (car e2)])
                             #,(mat (lambda ()
                                      #`(f (cdr e2) (cons v u) ...)))))))
                 (map
                  (lambda (id pvar)
                    (make-pattern-variable
                     (pattern-variable-identifier pvar)
                     id
                     (fx+ (pattern-variable-level pvar) 1)))
                  #'(u ...) ipvars)
                 catas))))))
      (define gen-map-values
        (lambda (proc-expr y* e n)
          (let f ([n n])
            (if (fxzero? n)
                #`(#,proc-expr #,e)
                (with-syntax ([(tmps ...)
                               (generate-temporaries y*)]
                              [(tmp ...)
                               (generate-temporaries y*)]
                              [e e])
                  #`(let f ([e* (reverse e)]
                            [tmps '()] ...)
                      (if (null? e*)
                          (values tmps ...)
                          (let ([e (car e*)]
                                [e* (cdr e*)])
                            (let-values ([(tmp ...)
                                          #,(f (fx- n 1))])
                              (f e* (cons tmp tmps) ...))))))))))
      (define gen-clause
        (lambda (k cl)
          (let*-values ([(pat guard-expr body)
                         (parse-clause cl)]
                        [(matcher pvars catas)
                         (gen-matcher #'e pat)])
	    (check-pattern-variables pvars)
	    (check-cata-bindings catas)
            (with-syntax ([quasiquote
                           (datum->syntax k 'quasiquote)]
                          [(x ...)
                           (map pattern-variable-identifier pvars)]
                          [(u ...)
                           (map pattern-variable-expression pvars)]
                          [(f ...)
                           (map cata-binding-proc-expr catas)]
                          [((y ...) ...)
                           (map cata-binding-value-id* catas)]
                          [(z ...)
                           (map cata-binding-identifier catas)]
                          [(tmp ...)
                           (generate-temporaries catas)])
              (with-syntax ([(e ...)
                             (map
                              (lambda (tmp y* z)
                                (let ([n
                                       (exists
                                        (lambda (pvar)
                                          (let ([x (pattern-variable-identifier pvar)])
                                            (and (bound-identifier=? x z)
                                                 (pattern-variable-level pvar))))
                                        pvars)])
                                  (gen-map-values tmp y* z n)))
                              #'(tmp ...) #'((y ...) ...) #'(z ...))])
                (matcher
                 (lambda ()
                   #`(let ([x u] ...)
                       (if #,guard-expr
                           (let ([tmp f] ...)
                             (let-values ([(y ...) e] ...)
                               (let-syntax ([quasiquote quasiquote-transformer])
                                 #,@body)))
                           (fail))))))))))
      (define gen-match
        (lambda (k cl*)
          (fold-right
           (lambda (cl rest)
             #`(let ([fail (lambda () #,rest)])
                 #,(gen-clause k cl)))
           #'(if #f #f)
           cl*)))
      (syntax-case stx ()
        [(k expr cl ...)
         #`(let loop ([e expr])
             #,(gen-match #'k #'(cl ...)))])))

  ;; Examples from R. Kent Dybvig's "Using Match"

  (define check~match-001
    (lambda ()
      (assert (equal? 3
		      (match '(a 17 37)
		        [(a ,x) 1]
		        [(b ,x ,y) 2]
		        [(a ,x ,y) 3])))))

  (define check~match-002
    (lambda ()
      (assert (equal? 629
		      (match '(a 17 37)
		        [(a ,x) (- x)]
		        [(b ,x ,y) (+ x y)]
		        [(a ,x ,y) (* x y)])))))

  (define check~match-003
    (lambda ()
      (assert (equal? '(17 37)
		      (match '(a 17 37) [(a ,x* ...) x*])))))

  (define check~match-004
    (lambda ()
      (assert (equal? '(a stitch in time saves nine)
		      (match '(say (a time) (stitch saves) (in nine))
		        [(say (,x* ,y*) ...) (append x* y*)])))))

  (define check~match-005
    (lambda ()
      (assert (equal? '((a e h j) ((b c d) (f g) (i) ()))
		      (match '((a b c d) (e f g) (h i) (j))
		        [((,x* ,y** ...) ...)
		         (list x* y**)])))))

  (define simple-eval
    (lambda (x)
      (match x
        [,i (guard (integer? i)) i]
        [(+ ,[x*] ...) (apply + x*)]
        [(* ,[x*] ...) (apply * x*)]
        [(- ,[x] ,[y]) (- x y)]
        [(/ ,[x] ,[y]) (/ x y)]
        [,x (assertion-violation 'simple-eval "invalid expression" x)])))

  (define check~match-006
    (lambda ()
      (assert (equal? 6
		      (simple-eval '(+ 1 2 3))))))

  (define check~match-008
    (lambda ()
      (assert (equal? 4
		      (simple-eval '(+ (- 0 1) (+ 2 3)))))))

  (define check~match-009
    (lambda ()
      (assert (guard (c
		      [(assertion-violation? c) #t])
	             (simple-eval '(- 1 2 3))))))

  (define translate
    (lambda (x)
      (match x
        [(let ((,var* ,expr*) ...) ,body ,body* ...)
         `((lambda ,var* ,body ,body* ...) ,expr* ...)]
        [,x (assertion-violation 'translate "invalid expression" x)])))

  (define check~match-010
    (lambda ()
      (assert (equal? '((lambda (x y) (+ x y)) 3 4)
		      (translate '(let ((x 3) (y 4)) (+ x y)))))))

  (define (f x)
    (match x
      [((,a ...) (,b ...)) `((,a . ,b) ...)]))

  (define check~match-011
    (lambda ()
      (assert (equal? '((1 . a) (2 . b) (3 . c))
		      (f '((1 2 3) (a b c)))))))

  (define check~match-012
    (lambda ()
      (assert (guard (c
		      [(assertion-violation? c) #t])
	             (f '((1 2 3) (a b)))))))

  (define (g x)
    (match x
      [(,a ,b ...) `((,a ,b) ...)]))

  (define check~match-013-0
    (lambda ()
      (assert (guard (c
		      [(assertion-violation? c) #t])
	             (g '(1 2 3 4))))))

  (define check~match-013-1
    (lambda ()
      (assert (equal? '((a 1) (b 2) (c 3)) (g '((a b c) 1 2 3))))))

  (define (h x)
    (match x
      [(let ([,x ,e1 ...] ...) ,b1 ,b2 ...)
       `((lambda (,x ...) ,b1 ,b2 ...)
         (begin ,e1 ...) ...)]))

  (define check~match-014
    (lambda ()
      (assert (equal? '((lambda (x y) (list x y))
		        (begin (write 'one) 3)
		        (begin (write 'two) 4))
		      (h '(let ((x (write 'one) 3) (y (write 'two) 4)) (list x y)))))))

  (define (k x)
    (match x
      [(foo (,x ...) ...)
       `(list (car ,x) ... ...)]))

  (define check~match-015
    (lambda ()
      (assert (equal? '(list (car a) (car b) (car c) (car d) (car e) (car f) (car g))
		      (k '(foo (a) (b c d e) () (f g)))))))

  (define parse1
    (lambda (x)
      (define Prog
        (lambda (x)
          (match x
            [(program ,[Stmt -> s*] ... ,[Expr -> e])
             `(begin ,s* ... ,e)]
            [,x (assertion-violation 'parse "invalid program" x)])))
      (define Stmt
        (lambda (x)
          (match x
            [(if ,[Expr -> e] ,[Stmt -> s1] ,[Stmt -> s2])
             `(if ,e ,s1 ,s2)]
            [(set! ,v ,[Expr -> e])
             (guard (symbol? v))
             `(set! ,v ,e)]
            [,x (assertion-violation 'parse "invalid statement" x)])))
      (define Expr
        (lambda (x)
          (match x
            [,v (guard (symbol? v)) v]
            [,n (guard (integer? n)) n]
            [(if ,[e1] ,[e2] ,[e3])
             `(if ,e1 ,e2 ,e3)]
            [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
            [,x (assertion-violation 'parse "invalid expression" x)])))
      (Prog x)))

  (define check~match-016
    (lambda ()
      (assert (equal? '(begin (set! x 3) (+ x 4))
		      (parse1 '(program (set! x 3) (+ x 4)))))))

  (define parse2
    (lambda (x)
      (define Prog
        (lambda (x)
          (match x
            [(program ,[Stmt -> s*] ... ,[(Expr '()) -> e])
             `(begin ,s* ... ,e)]
            [,x (assertion-violation 'parse "invalid program" x)])))
      (define Stmt
        (lambda (x)
          (match x
            [(if ,[(Expr '()) -> e] ,[Stmt -> s1] ,[Stmt -> s2])
             `(if ,e ,s1 ,s2)]
            [(set! ,v ,[(Expr '()) -> e])
             (guard (symbol? v))
             `(set! ,v ,e)]
            [,x (assertion-violation 'parse "invalid statement" x)])))
      (define Expr
        (lambda (env)
          (lambda (x)
            (match x
              [,v (guard (symbol? v)) v]
              [,n (guard (integer? n)) n]
              [(if ,[e1] ,[e2] ,[e3])
               (guard (not (memq 'if env)))
               `(if ,e1 ,e2 ,e3)]
              [(let ([,v ,[e]]) ,[(Expr (cons v env)) -> body])
               (guard (not (memq 'let env)) (symbol? v))
               `(let ([,v ,e]) ,body)]
              [(,[rator] ,[rand*] ...)
               `(call ,rator ,rand* ...)]
              [,x (assertion-violation 'parse "invalid expression" x)]))))
      (Prog x)))

  (define check~match-017
    (lambda ()
      (assert (equal? '(begin
		         (let ([if (if x list values)])
		           (call if 1 2 3)))
		      (parse2
		       '(program
		         (let ([if (if x list values)])
		           (if 1 2 3))))))))

  (define check~match-018
    (lambda ()

      (define split
        (lambda (ls)
          (match ls
            [() (values '() '())]
            [(,x) (values `(,x) '())]
            [(,x ,y . ,[odds evens])
             (values (cons x odds)
                     (cons y evens))])))

      (assert (equal? '((a c e) (b d f))
		      (call-with-values
		          (lambda ()
		            (split '(a b c d e f)))
		        list)))))

  ;; Extra tests

  (define check~match-019
    (lambda ()
      (assert (match 'a
                [(,x) #f]
                [,_ #t]))))

  (define check~match-020
    (lambda ()
      (assert (match 'a
                [(,x) #f]
                [else #t]))))


  )

;; Local Variables:
;; mode: scheme
;; End:
