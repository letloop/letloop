(library (letloop dxdb dd)
  (export make-dd
          ~check-dd-999)

  (import (chezscheme)
          (letloop okvs))

  (define-record-type* <dd>
    (make-dd-base dbx)
    dd?
    (dbx dd-dbx))

  (define make-dd
    (lambda (dbx)
      ...))

  (define dd-from-okvs
    (lambda (okvs)
      ...))

  (define okvs->dd
    (lambda ...
      ...))

  (define dd->okvs
    (lambda ...
      ...))

  )
