(library (letloop lsm1 okvs)
  (export make-okvs
          okvs-empty?
          okvs?
          okvs-error?
          okvs-close
          okvs-cursor?
          okvs-transaction?
          okvs-transaction-read-only?
          okvs-handle?
          okvs-key-max-length
          okvs-value-max-length
          okvs-transaction-timeout
          make-okvs-parameter
          okvs-parameterize
          okvs-begin-hook
          okvs-pre-commit-hook
          okvs-post-commit-hook
          okvs-rollback-hook
          call-with-okvs-transaction
          call-with-okvs-transaction-read-only
          okvs-approximate-key-count
          okvs-approximate-byte-count
          okvs-set!
          okvs-clear!
          call-with-okvs-cursor
          okvs-next
          okvs-previous
          okvs-key
          okvs-value
          okvs-query)

  (import (chezscheme) (letloop gamma) (letoop okvs))

  (define-syntax define-gamma
    (syntax-rules ()
      ((define-gamma out name predicate? procedure)
       (define out (gamma name (lambda args (and (pair? args) (predicate? (car args)))) procedure)))))

  (define-gamma okvs-lsm1-empty? okvs-empty? okvs-lsm1? (lambda
  (define-gamma okvs?)
  (define-gamma okvs-error?)
  (define-gamma okvs-close)
  (define-gamma okvs-cursor?)
  (define-gamma okvs-transaction?)
  (define-gamma okvs-transaction-read-only?)

  (define okvs-handle?
    (lambda (object)
      (or (okvs? object)
          (okvs-transaction? object)
          (okvs-transaction-read-only? object)
          (okvs-cursor? object))))

  (define-gamma okvs-key-max-length)
  (define-gamma okvs-value-max-length)
  (define-gamma okvs-transaction-timeout)
  (define-gamma make-okvs-parameter)
  (define-gamma okvs-parameterize)
  (define-gamma okvs-begin-hook)
  (define-gamma okvs-pre-commit-hook)
  (define-gamma okvs-post-commit-hook)
  (define-gamma okvs-rollback-hook)
  (define-gamma call-with-okvs-transaction)
  (define-gamma call-with-okvs-transaction-read-only)
  (define-gamma okvs-approximate-key-count)
  (define-gamma okvs-approximate-byte-count)
  (define-gamma okvs-set!)
  (define-gamma okvs-clear!)
  (define-gamma call-with-okvs-cursor)
  (define-gamma okvs-next)
  (define-gamma okvs-previous)
  (define-gamma okvs-key)
  (define-gamma okvs-value)
  (define-gamma okvs-query))
