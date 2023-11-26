(library (letloop wiki)
  (export ~check-letloop-wiki-000
          ;; ~check-letloop-wiki-001
          ;; ~check-letloop-wiki-002
          ;; ~check-letloop-wiki-003
          ;; ~check-letloop-wiki-004
          ;; ~check-letloop-wiki-005
          )
  (import (chezscheme)
          (letloop match)
          (letloop argon2)
          (letloop lsm1 okvs)
          (letloop commonmark)
          (letloop flow)
          (letloop http)
          (letloop html)
          (letloop byter)
          (scheme generator)
          (letloop www)
          (letloop literally))

  (define pk
    (lambda args
      (write args) (newline)
      (flush-output-port)
      (car (reverse args))))
  
  (define call-with-database
    ;; Execute PROC within a transaction, passing a pseudo transaction
    ;; object as first argument, followed by ARGS...
    (lambda (proc . args)
      (define db (make-okvs "wiki-symbolic.db"))
      (call-with-values (lambda () (call-with-okvs-transaction db
                                     (lambda (tx) (apply proc tx args))))
        (lambda args
          (okvs-close db)
          (apply values args)))))

  ;; wiki procedures

  (define and=>
    (lambda (v proc)
      (if v (proc v) #f)))
  
  (define LETLOOP-WIKI-SECRET
    (or (and=> (getenv "LETLOOP_WIKI_SECRET")
               string->utf8)
        (byter-random 1024)))

  (define wiki-secret
    (lambda ()
      LETLOOP-WIKI-SECRET))
 
  (define wiki-sign-up
    (lambda (username password)
      ;; prepare arguments outside the transaction to keep the
      ;; transaction latency to a minimum, minimize race conditions,
      ;; hence maximize transactions success, and throughput.
      (let ((username* (string->utf8 username))
            (password* (byter-append (wiki-secret) (string->utf8 password)))
            (salt (byter-random 1024)))
        (call-with-database
         (lambda (tx)
           ;; To make sur usernames are unique do check and insert
           ;; within the same transaction.
           (if (okvs-query tx username*)
               #f
               (okvs-set! tx
                          username*
                          (argon2id-encode salt password*))))))))

  (define wiki-sign-in
    (lambda (username password)
      (let ((encoded (call-with-database
                      (lambda (tx)
                        (okvs-query tx (string->utf8 username)))))
            (password* (byter-append (wiki-secret) (string->utf8 password))))
        
        (if (not encoded)
            #f
            (argon2id-verify encoded password*)))))

  (define ~check-letloop-wiki-000
    (lambda ()
      (define password "#too-much-positive-fun-3301!")
      ;; can not sign in without prior sign up
      (assert (not (wiki-sign-in "amirouche" password)))
      ;; can sign up
      (assert (wiki-sign-up "amirouche" password))
      ;; can not sign up twice
      (assert (not (wiki-sign-up "amirouche" password)))
      ;; can not sign up twice even with a different password
      (assert (not (wiki-sign-up "amirouche" "#;>(x4u:8! & do not think about it.)")))
      ;; can not sign in with the wrong password
      (assert (not (wiki-sign-in "amirouche" "#;>(x4u:8! & do not think about it.)")))
      ;; can sign in
      (assert (wiki-sign-in "amirouche" password))))

  (define wiki-upload
     ;; upload a new page's code
    (lambda (

    )

  (define wiki-download) ;; download a page's code

  (define wiki-eval) ;; aka. GET page

  (define wiki-apply) ;; aka. POST page

  (define wiki-history)

  (define wiki-add)

  (define wiki-query)

  )
