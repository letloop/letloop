#!chezscheme
(library (letloop dxdb dbx)
  (export make-dbx
          dbx-page-bytes
          dbx-ref
          dbx-bytes
          dbx-set!
          dbx-push!
          dbx-pop!
          dbx-sync!
          make-dbx-chapter-x
          dbx-chapter-x-ref
          dbx-chapter-x-append!
          dbx-chapter-x-query
          ~check-dbx-000
          ~check-dbx-001
          ~check-dbx-002
          ~check-dbx-003
          )
  (import (chezscheme)
          (letloop r999)
          (letloop byter)
          (letloop entangle))

  ;;
  ;; XXX: DO NOT CACHE PAGES: READ, WRITE, AND SYNC ON NEED BY NEED
  ;; BASIS.  IN OTHER WORDS, DO NOT PASS AROUND BYTEVECTORS.
  ;;
  ;; WHY? Because it will be easier to discover patterns if all the
  ;; code use the same simple and slow code.
  ;;
  ;; XXX: DO NOT CACHE.
  ;;

  ;; TODO: ADD CHECKSUM.

  (define pk*
    (lambda args
      (write args)
      (newline)
      (car (reverse args))))

  (define-syntax pk
    (syntax-rules ()
      ((pk args ...)
       (pk* 'args ... args ...))))

  (define-record-type* <dbx>
    (make-dbx-base fd page-bytes)
    dbx?
    (fd dbx-fd)
    (page-bytes dbx-page-bytes))

  (define recycler-previous-pid
    (lambda (dbx pid)
      (bytevector-u64-ref (dbx-ref dbx pid) 0 'big)))

  (define recycler-previous-pid!
    (lambda (dbx pid previous)
      (define out (dbx-ref dbx pid))
      (bytevector-u64-set! out 0 previous 'big)
      (dbx-set! dbx pid out)
      (dbx-sync! dbx pid)))

  (define recycler-head
    (lambda (dbx pid)
      (bytevector-u64-ref (dbx-ref dbx pid) 8 'big)))

  (define recycler-head!
    (lambda (dbx pid roffset)
      (define out (dbx-ref dbx pid))
      (bytevector-u64-set! out 8 roffset 'big)
      (dbx-set! dbx pid out)
      (dbx-sync! dbx pid)))

  (define recycler-empty?
    (lambda (dbx)
      ;; the current recycler page is the zeroth itself
      (and (= (recycler-previous-pid dbx 0) 0)
           ;; the head of the zeroth page is at the end
           ;; of the page, that means it is empty.
           (= (recycler-head dbx 0) (dbx-page-bytes dbx)))))

  (define dbx-bytes
    (lambda (dbx)
      (entangle-bytes (dbx-fd dbx))))

  (define make-dbx
    (lambda (filepath page-bytes)
      (define fd (entangle-open filepath
                                (list 'entangle-file-create
                                      'entangle-file-read-write)))
      (define dbx (make-dbx-base fd page-bytes))

      (when (= 0 (dbx-bytes dbx))
        ;; new db, initialize the recycler.
        (let ((zeroth (make-bytevector (dbx-page-bytes dbx))))
          ;; spurious dbx-set! to be able to use procedures of the
          ;; recycler.
          (dbx-set! dbx 0 zeroth)
          ;; the previous page of the zeroth page is itself at the
          ;; beginning.
          (recycler-previous-pid! dbx 0 0)
          ;; At roffset 8 is stored the roffset of the head, when the page
          ;; is full it is equal to 16. Items in a recycler page start at
          ;; the end. So the page schema is:
          ;;
          ;;   { [previous page pid] [roffset of the head] ... [head] ... }
          ;;
          ;; The first uint64 is page id (pid) of the previous page,
          ;; except in the case of the zeroth page, where the first uint64
          ;; store the pid of the last page.
          (recycler-head! dbx 0 (dbx-page-bytes dbx))))
      dbx))

  (define dbx-set!
    (lambda (dbx pid bytevector)
      (entangle-pwrite (dbx-fd dbx)
                       (* pid (dbx-page-bytes dbx))
                       bytevector)))

  (define dbx-ref
    (lambda (dbx pid)
      (define out (entangle-pread (dbx-fd dbx)
                                  (* pid (dbx-page-bytes dbx))
                                  (dbx-page-bytes dbx)))

      (unless (= (dbx-page-bytes dbx) (bytevector-length out))
        (pk 'warning "page is smaller than dbx-page" (dbx-page-bytes dbx)))
      out))

  (define dbx-sync!
    (lambda (dbx pid)
      (entangle-sync (dbx-fd dbx)
                     (* pid (dbx-page-bytes dbx))
                     (dbx-page-bytes dbx))))

  (define recycler-ref
    (lambda (dbx pid roffset)
      (bytevector-u64-ref (dbx-ref dbx pid) roffset 'big)))

  (define recycler-set!
    (lambda (dbx pid roffset v)
      (define x (dbx-ref dbx pid))
      (bytevector-u64-set! x roffset v 'big)
      (dbx-set! dbx pid x)
      (dbx-sync! dbx pid)))

  (define recycler-pop!
    (lambda (dbx)
      (let* ((pid (recycler-previous-pid dbx 0))
             (roffset (recycler-head dbx pid)))
        (if (= roffset (dbx-page-bytes dbx))
            (if (= pid 0)
                ;; the recycler is empty, return the pid of the page
                ;; at the very edge end of the file
                (let ((pid (/ (dbx-bytes dbx) (dbx-page-bytes dbx))))
                  (dbx-set! dbx pid (make-bytevector (dbx-page-bytes dbx) 0))
                  pid)
                (begin
                  ;; The current empty, and is not the zeroth page.
                  ;; Return pid as a free page, but before, fix the
                  ;; recycler: set the previous pid of the zeroth page
                  ;; to the page before pid.
                  (recycler-previous-pid! dbx 0 (recycler-previous-pid dbx pid))
                  pid))
            (let ((fid (recycler-ref dbx pid roffset)))
              (recycler-head! dbx pid (+ roffset 8))
              fid)))))

  (define recycler-push!
    (lambda (dbx pid)
      (let* ((rid (recycler-previous-pid dbx 0))
             (roffset (recycler-head dbx rid)))
        (if (not (= roffset 16))
            (let ((roffset* (- roffset 8)))
              (recycler-set! dbx rid roffset* pid)
              (recycler-head! dbx rid roffset*))
            ;; the recycler rid is full
            (let ((new (recycler-pop! dbx)))
              (recycler-previous-pid! dbx new rid)
              (recycler-previous-pid! dbx 0 new)
              (recycler-set! dbx new (- (dbx-page-bytes dbx) 8) rid)
              (recycler-head! dbx new (- (dbx-page-bytes dbx) 8)))))))

  (define dbx-pop! recycler-pop!)
  (define dbx-push! recycler-push!)

  ;; Checks

  (define call-with-temporary-filepath
    (lambda (prefix proc)
      (define stdlib (load-shared-object #f))

      (define mkstemp
        (foreign-procedure "mkstemp" (string) int))

      (define close
        (foreign-procedure "close" (int) int))

      (define (make-temporary-filepath prefix)
        (let ((input (string-append prefix "-XXXXXX")))
          (close (mkstemp input))
          input))

      (define filepath (make-temporary-filepath prefix))

      (call-with-values (lambda () (proc filepath))
        (lambda args
          (delete-file filepath)
          (apply values args)))))

  (define ~check-dbx-000
    (lambda ()
      (define filepath #f)
      (call-with-temporary-filepath "dbx-check"
        (lambda (filepath*)
          ;;
          ;; For an unknown reason sometime it fails:
          ;;
          ;; (assert (not (file-exists? filepath*)))
          ;;
          (set! filepath filepath*)
          (with-entangle
           (let ((fd (entangle-open filepath
                                    (list 'entangle-file-create
                                          'entangle-file-read-write))))
             (entangle-pwrite fd 0 (bytevector 101 13 37))
             (entangle-sync fd 0 3)
             (assert (equal? (bytevector 101 13 37) (entangle-pread fd 0 3)))))))
      (not (file-exists? filepath))))

  (define ~check-dbx-001
    (lambda ()
      (call-with-temporary-filepath "dbx-check"
        (lambda (filepath)
          (with-entangle
           (let* ((dbx (make-dbx filepath 128))
                  (expected (make-bytevector (dbx-page-bytes dbx))))
             (dbx-set! dbx 0 expected)
             (assert (equal? expected (dbx-ref dbx 0)))))))))

  (define ~check-dbx-002
    (lambda ()
      (call-with-temporary-filepath "dbx-check"
        (lambda (filepath)
          (with-entangle
           (let* ((dbx (make-dbx filepath 1024))
                  (expected (u8-list->bytevector
                             (map (lambda _ (random 256)) (iota (dbx-page-bytes dbx)))))
                  (pid (dbx-pop! dbx)))
             (assert (= pid 1))
             (dbx-set! dbx
                       pid
                       expected)
             (assert (equal? expected
                             (dbx-ref dbx 1)))
             (dbx-push! dbx 1)
             (assert (= 1 (dbx-pop! dbx)))))))))

  (define make-dbx-chapter-x
    (case-lambda
     ((dbx)
      (define pid (dbx-pop! dbx))
      (define bv (dbx-ref dbx pid))
      ;; Wallou sentinel...
      (bytevector-u64-set! bv 0 1 'big)
      ;; Wallou next page...
      (bytevector-u64-set! bv (- (dbx-page-bytes dbx) 8) 0 'big)
      (dbx-set! dbx pid bv)
      (dbx-sync! dbx pid)
      pid)
     ((dbx bytevector)

      (define page-generator
        (lambda (bytevector length)
          (define start 0)
          (lambda ()
            (if (< start (bytevector-length bytevector))
                (eof-object)
                (if (<= (- (bytevector-length bytevector) start) length)
                    (byter-concatenate (list (byter-slice bytevector start (bytevector-length bytevector))
                                             (make-bytevector (- length
                                                                 (- (bytevector-length bytevector) start))
                                                              0)))
                    (let ((offset start))
                      (set! start (+ start length))
                      (byter-slice bytevector offset (+ offset length))))))))

      (define generator-for-each
        (lambda (p g)
          (let loop ()
            (let ((o (g)))
              (if (eof-object? o)
                  (eof-object)
                  (begin
                    (p o)
                    (loop)))))))

      (define x (make-dbx-chapter-x dbx))

      (generator-for-each (lambda (bv)
                            (dbx-chapter-x-append! dbx x bv))
                          (page-generator bytevector (dbx-page-bytes dbx)))

      x)))

  (define dbx-chapter-x-ref
    (lambda (dbx pid offset)
      (bytevector-u64-ref (dbx-chapter-x-query dbx pid offset 8)
                          0 'big)))

  (define dbx-chapter-x-append!
    (lambda (dbx xid bv)
      (define ignore (assert (= (dbx-page-bytes dbx) (bytevector-length bv))))
      (define pid (dbx-pop! dbx))
      (dbx-set! dbx pid bv)
      (dbx-sync! dbx pid)
      (dbx-chapter-x-index-set! dbx xid (dbx-chapter-x-index-head dbx xid) pid)
      (dbx-chapter-x-index-head! dbx xid (+ (dbx-chapter-x-index-head dbx xid) 1))))

  (define bytevector-concatenate
    (lambda (bvs)
      (let* ((total (apply fx+ (map bytevector-length bvs)))
             (out (make-bytevector total)))
        (let loop ((bvs bvs)
                   (index 0))
          (unless (null? bvs)
            (bytevector-copy! (car bvs) 0 out index (bytevector-length (car bvs)))
            (loop (cdr bvs) (fx+ index (bytevector-length (car bvs))))))
        out)))

  (define dbx-chapter-x-index-head
    (lambda (dbx xid)
      (dbx-chapter-x-index-ref dbx xid 0)))

  (define dbx-chapter-x-index-head!
    (lambda (dbx xid v)
      (dbx-chapter-x-index-set! dbx xid 0 v)))

  (define dbx-chapter-x-index-next
    (lambda (dbx pid)
      (bytevector-u64-ref (dbx-ref dbx pid) (- (dbx-page-bytes dbx) 8) 'big)))

  (define dbx-chapter-x-index-next!
    (lambda (dbx pid v)
      (bytevector-u64-set! (dbx-ref dbx pid) (- (dbx-page-bytes dbx) 8) v 'big)))

  (define dbx-chapter-x-index-ref
    (lambda (dbx pid i)
      (bytevector-u64-ref (dbx-ref dbx pid)
                          (* 8 i)
                          'big)))

  (define dbx-chapter-x-index-set!
    (lambda (dbx pid i v)
      (define bv (dbx-ref dbx pid))
      (bytevector-u64-set! bv (* 8 i) v 'big)
      (dbx-set! dbx pid bv)
      (dbx-sync! dbx pid)))

  (define dbx-chapter-x-query
    (lambda (dbx pid offset length)
      ;; A chapter is virtual bytevector. offset is translated into a
      ;; pid + roffset, inside that bytevector sliced into pages of
      ;; size dbx-page-bytes. The index of the slice is xppid0. xppid1
      ;; is the index of the slice containing the last byte.
      (define-values (xppid0 roffset0) (div-and-mod offset (dbx-page-bytes dbx)))
      (define-values (xppid1 roffset1) (div-and-mod (+ offset length) (dbx-page-bytes dbx)))

      ;; the xppid is the pid of virtual page inside the whole
      ;; chapter's bytevector, it is also the index in the chapter
      ;; prelude where the associated dbx pid is stored. The prelude
      ;; is built as linked list of u64, except the last u64 that is
      ;; the pid of the next prelude page.

      ;; compute the prelude page index, and roffset of both
      (define-values (i0 i0-offset) (div-and-mod xppid0 (/ (dbx-page-bytes dbx) 8)))
      (define-values (i1 i1-offset) (div-and-mod xppid1 (/ (dbx-page-bytes dbx) 8)))

      (let loop0 ((i i0)
                  (pid pid))

        (if (not (zero? i))
            (loop0 (fx- i 1)
                   (dbx-chapter-x-index-next dbx pid))
            (let loop1 ((k (+ i0 1))
                        (i i0-offset)
                        (pid pid)
                        (out '()))
              (cond
               ((and (= 1 (+ i1 1)) (= i i1-offset)) (bytevector-concatenate (reverse out)))
               ((= i (- (dbx-page-bytes dbx) 8))
                (loop1 (+ k 1)
                       0
                       (dbx-chapter-x-index-next dbx k)
                       out))
               (else
                (loop1 k
                       (+ i 1)
                       pid
                       (cons (dbx-ref dbx (dbx-chapter-x-index-ref dbx pid (+ i 1)))
                             out)))))))))

  (define ~check-dbx-003
    (lambda ()
      (call-with-temporary-filepath "dbx-check"
        (lambda (filepath)
          (define n 4096)
          (with-entangle
           (let* ((dbx (make-dbx filepath n))
                  (x (make-dbx-chapter-x dbx))
                  (bv0 (make-bytevector (dbx-page-bytes dbx) 1))
                  (bv1 (make-bytevector (dbx-page-bytes dbx) 5))
                  (bv2 (make-bytevector (dbx-page-bytes dbx) 9)))
             (dbx-chapter-x-append! dbx x bv0)
             (dbx-chapter-x-append! dbx x bv1)
             (dbx-chapter-x-append! dbx x bv2)
             (assert
              (equal? (dbx-chapter-x-query dbx x 0 (* 3 n))
                      (bytevector-concatenate (list bv0 bv1 bv2))))))))))

  )
