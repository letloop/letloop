(library (letloop wiki)
  (export letloop-wiki
          ~check-letloop-wiki-000-sign-up-and-sign-in
          ~check-letloop-wiki-001-upload-download
          ~check-letloop-wiki-002-upload-eval
          ~check-letloop-wiki-003-upload-apply
          ~check-letloop-wiki-004-upload-history
          ~check-letloop-wiki-004-add-query
          )
  (import (chezscheme)
          (letloop match)
          (letloop lsm1 okvs)
          (letloop commonmark)
          (letloop entangle)
          (letloop http)
          (letloop html)
          (letloop byter)
          (scheme generator)
          (letloop www)
          (letloop literally))

  (define LETLOOP-SECRET
    (or (string->utf8 (getenv "LETLOOP_SECRET"))
        (byter-random 1024)))

  (define wiki-secret
    (lambda ()
      LETLOOP-SECRET))

  (define call-with-database
    (lambda (proc . args)
      (define db (make-okvs "wiki-symbolic.db"))
      (call-with-values (lambda () (call-with-okvs-transaction db
                                     (lambda (tx) (apply proc tx args))))
        (lambda args
          (okvs-close db)
          (apply values args)))))

  (define wiki-sign-up
    (lambda (username password)
      (call-with-database
       (lambda (tx)
         (if (okvs-query tx username*)
             #f
             (okvs-set! tx (string->utf8 username)
                        (argon2id (byter-random 1024) password)))))))

  (define wiki-sign-in
    (lambda (username password)
      (define encoded (okvs-query tx (string->utf8 username)))
      (if (not encoded)
          #f
          (argon2id-verify encoded password))))

  (define wiki-upload) ;; upload a new page's code

  (define wiki-download) ;; download a page's code

  (define wiki-eval) ;; aka. GET page

  (define wiki-apply) ;; aka. POST page

  (define wiki-history)

  (define wiki-add)

  (define wiki-query)

  (define unknown-magic 1536)
