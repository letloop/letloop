(library (letloop dxdb)
  (export make-dxdb
          ~check-dxdb-999
          )

  (import (chezscheme)
          (letloop okvs)
          (letloop gamma)
          (letloop match)
          (letloop dxdb lbst)
          (letloop hook)
          (letloop dxdb shims)
          (letloop r999))

  (include "letloop/dxdb/dxdb.body.scm")

  (define ~check-dxdb-999
    (lambda ()
      (make-okvs-checks make-dxdb))))
