#!chezscheme
;; sqlite3
;;
;;; Comment:
;;
;; - 2021/09: initial version
;; - 2022/04: debug
;;
;; Read also:
;;
;; - See the C introduction at https://sqlite.org/cintro.html
;; - Header is available on github at https://git.io/JuRBE
;;
(library (letloop sqlite3)
  (export
   sqlite3-open
   sqlite3-close
   sqlite3-prepare
   sqlite3-reset
   sqlite3-bind-blob
   sqlite3-step
   sqlite3-column-blob
   sqlite3-column-bytes
   sqlite3-column-int

   ~check-sqlite3-000)

  (import (chezscheme))

  ;; helpers

  (define pk
    (lambda args
      (write args (current-error-port))
      (newline (current-error-port))
      (car (reverse args))))

  (define call-with-lock
    (lambda (object thunk)
      (lock-object object)
      (call-with-values thunk
        (lambda args
          (unlock-object object)
          (apply values args)))))

  ;; ffi helpers

  (define make-double-pointer
    (lambda ()
      (make-bytevector 8)))

  (define pointer-dereference
    (lambda (pointer)
      (bytevector-u64-native-ref pointer 0)))

  ;; sqlite3 bindings

  (define libsqlite3 (load-shared-object "libsqlite3.so.0"))

  (define sqlite3-libversion
    (let ((func (foreign-procedure "sqlite3_libversion" () string)))
      (lambda ()
        (func))))

  (define sqlite3-libversion-number
    (let ((func (foreign-procedure "sqlite3_libversion_number" () string)))
      (lambda ()
        (func))))

  (define sqlite3-threadsafe
    (let ((func (foreign-procedure "sqlite3_threadsafe" () int)))
      (lambda ()
        (func))))

  (define sqlite3-close
    (let ((func (foreign-procedure "sqlite3_close" (void*) int)))
      (lambda (database)
        (func database))))

  (define-syntax check
    (syntax-rules ()
      ((check who code)
       ;; XXX: SQLite3 can be configured at runtime to return more
       ;; precise error codes, see sqlite3.h.

       (unless (fxzero? code)
         (case code
           ((1) (error who "generic error"))
           ((2) (error who "internal logic error in SQLite"))
           ((3) (error who "access permission denied"))
           ((4) (error who "callback routine requested an abort"))
           ((5) (error who "the database file is locked"))
           ((6) (error who "a table in the database is locked"))
           ((7) (error who "A malloc() failed"))
           ((8) (error who "attempt to write a readonly database"))
           ((9) (error who "operation terminated by sqlite3_interupt()"))
           ((10) (error who "some kind of disk I/O error occured"))
           ((11) (error who "the database disk image is malformed"))
           ((12) (error who "unknoan opcode in sqlite3_file_control()"))
           ((13) (error who "insertion failed because database is full"))
           ((14) (error who "unable to open the database file"))
           ((15) (error who "database lock protocol error"))
           ((16) (error who "internal use only"))
           ((17) (error who "the database schema changed"))
           ((18) (error who "string or BLOB exceeds size limit"))
           ((19) (error who "abort due to contraint violation"))
           ((20) (error who "data type mismatch"))
           ((21) (error who "library used incorrectly"))
           ((22) (error who "uses OS features not supported on host"))
           ((23) (error who "authorization denied"))
           ((24) (error who "not used"))
           ((25) (error who "2nd parameter to sqlite3_bind out of range"))
           ((26) (error who "file opened that is not a database file"))
           ((27) (error who "notification from sqlite3_log()"))
           ((28) (error who "warnings from sqlite3_log()"))
           #;((100) (error who "sqlite: sqlite3_step() has another row ready"))
           #;((101) (error who "sqlite: sqlite3_step() has finished executing")))))))

  ;; Flags for file open operations
  (define SQLITE_OPEN_READONLY #x00000001) ;; /* Ok for sqlite3_open_v2() */
  (define SQLITE_OPEN_READWRITE #x00000002) ;; /* Ok for sqlite3_open_v2() */
  (define SQLITE_OPEN_CREATE #x00000004) ;; /* Ok for sqlite3_open_v2() */
  (define SQLITE_OPEN_DELETEONCLOSE #x00000008) ;; /* VFS only */
  (define SQLITE_OPEN_EXCLUSIVE #x00000010) ;; /* VFS only */
  (define SQLITE_OPEN_AUTOPROXY #x00000020) ;; /* VFS only */
  (define SQLITE_OPEN_URI #x00000040) ;; /* Ok for sqlite3_open_v2() */
  (define SQLITE_OPEN_MEMORY #x00000080) ;; /* Ok for sqlite3_open_v2() */
  (define SQLITE_OPEN_MAIN_DB #x00000100) ;; /* VFS only */
  (define SQLITE_OPEN_TEMP_DB #x00000200) ;; /* VFS only */
  (define SQLITE_OPEN_TRANSIENT_DB #x00000400) ;; /* VFS only */
  (define SQLITE_OPEN_MAIN_JOURNAL #x00000800) ;; /* VFS only */
  (define SQLITE_OPEN_TEMP_JOURNAL #x00001000) ;; /* VFS only */
  (define SQLITE_OPEN_SUBJOURNAL #x00002000) ;; /* VFS only */
  (define SQLITE_OPEN_SUPER_JOURNAL #x00004000) ;; /* VFS only */
  (define SQLITE_OPEN_NOMUTEX #x00008000) ;; /* Ok for sqlite3_open_v2() */
  (define SQLITE_OPEN_FULLMUTEX #x00010000) ;; /* Ok for sqlite3_open_v2() */
  (define SQLITE_OPEN_SHAREDCACHE #x00020000) ;; /* Ok for sqlite3_open_v2() */
  (define SQLITE_OPEN_PRIVATECACHE #x00040000) ;; /* Ok for sqlite3_open_v2() */
  (define SQLITE_OPEN_WAL #x00080000) ;; /* VFS only */
  (define SQLITE_OPEN_NOFOLLOW #x01000000) ;; /* Ok for sqlite3_open_v2() */

  (define sqlite3-open
    (let ((func (foreign-procedure "sqlite3_open" (string u8*) int)))
      (lambda (filename)
        (let ((out (make-double-pointer)))
          (check 'sqlite3-open (call-with-lock out (lambda () (func filename out))))
          (pointer-dereference out)))))

  (define SQLITE3-PREPARE-PERSISTENT 1)

  (define sqlite3-prepare
    (let ((func (foreign-procedure "sqlite3_prepare_v3"
                                   (void* string int unsigned-int u8* int)
                                   int)))
      (lambda (db sql)
        (let ((out (make-double-pointer)))
          (check 'sqlite3-prepare (call-with-lock out (lambda () (func db sql (string-length sql) SQLITE3-PREPARE-PERSISTENT out 0))))
          (pointer-dereference out)))))

  (define SQLITE-STATIC 0)

  (define sqlite3-bind-blob
    (let ((func (foreign-procedure "sqlite3_bind_blob" (void* int u8* int int) int)))
      (lambda (statement index bytevector)
        (check 'sqlite3-bind-blob
               (call-with-lock bytevector
                 (lambda ()
                   (func statement
                         index
                         bytevector
                         (bytevector-length bytevector)
                         SQLITE-STATIC)))))))

  (define sqlite3-clear-bindings
    (let ((func (foreign-procedure "sqlite3_clear_bindings" (void*) int)))
      (lambda (statement)
        (check 'sqlite3-clear-bindings (func statement)))))

  (define sqlite3-step
    (let ((func (foreign-procedure "sqlite3_step" (void*) int)))
      (lambda (statement)
        ;; Returns #true if there is still rows to iterate
        (let ((code (func statement)))
          (cond
           ;; they are more rows.
           ((fx=? code 100) #t)
           ;; the iterator is finished; there is no more rows.
           ((fx=? code 101) #f)
           ;; unknown
           (else (check 'sqlite3-step code) #f))))))

  (define sqlite3-column-blob
    (let ((func (foreign-procedure "sqlite3_column_blob" (void* int) void*)))
      (lambda (statement index)
        (let ((address (func statement index)))
          (if (fx=? address 0)
              #f
              (let* ((length (sqlite3-column-bytes statement index))
                     (out (make-bytevector length)))
                (let loop ((index length))
                  (unless (fxzero? index)
                    (let ((index (fx- index 1)))
                      (bytevector-u8-set! out
                                          index
                                          (foreign-ref 'unsigned-8 address index))
                      (loop index))))
                out))))))

  (define sqlite3-column-int
    (let ((func (foreign-procedure "sqlite3_column_int64" (void* integer-64) int)))
      (lambda (statement index)
        (func statement index))))

  (define sqlite3-column-bytes
    (let ((func (foreign-procedure "sqlite3_column_bytes" (void* int) int)))
      (lambda (statement index)
        (func statement index))))

  (define sqlite3-finalize
    (let ((func (foreign-procedure "sqlite3_finalize" (void*) int)))
      (lambda (statement)
	(check 'sqlite3-finalize (func statement)))))

  (define sqlite3-reset
    (let ((func (foreign-procedure "sqlite3_reset" (void*) int)))
      (lambda (statement)
	(check 'sqlite3-reset (func statement)))))

  (define stdlib (load-shared-object #f))

  (define mkdtemp
    (foreign-procedure "mkdtemp" (string) string))

  (define (make-temporary-directory prefix)
    (let ((input (string-append prefix "-XXXXXX")))
      (mkdtemp input)))

  (define ~check-sqlite3-000
    (lambda ()
      (define sqlite3 (sqlite3-open
                       (string-append (make-temporary-directory "/tmp/letloop-sqlite3")
                                      "/database.sqlite3")))

      (define ignore-create-table
        (let ((sql (sqlite3-prepare sqlite3
                                    "CREATE TABLE IF NOT EXISTS blob (name BLOB NOT NULL, blob BLOB NOT NULL);")))
          (sqlite3-step sql)
          (sqlite3-reset sql)))

      (define SQL_BEGIN_TRANSACTION (sqlite3-prepare sqlite3 "BEGIN TRANSACTION;"))

      (define SQL_END_TRANSACTION (sqlite3-prepare sqlite3 "END TRANSACTION;"))

      (define SQL_SET (sqlite3-prepare sqlite3 "INSERT INTO blob (name, blob) VALUES (?, ?);"))

      (define SQL_REF (sqlite3-prepare sqlite3 "SELECT blob FROM blob WHERE name = ?"))

      ;; Inside a transaction, associate hello-world to a blob.
      (define ignore-insert-hello-blob
        (let ()
          ;; begin transaction
          (sqlite3-step SQL_BEGIN_TRANSACTION)
          (sqlite3-reset SQL_BEGIN_TRANSACTION)

          ;; bind variables to values
          (sqlite3-bind-blob SQL_SET 1 (string->utf8 "hello-world"))
          (sqlite3-bind-blob SQL_SET 2 #vu8(3 13 37))
          (sqlite3-step SQL_SET)
          (sqlite3-reset SQL_SET)

          (sqlite3-step SQL_END_TRANSACTION)
          (sqlite3-reset SQL_END_TRANSACTION)))

      (define ignore-assert-hello-blob
        (let ()
          ;; begin transaction
          (sqlite3-step SQL_BEGIN_TRANSACTION)
          (sqlite3-reset SQL_BEGIN_TRANSACTION)

          ;; bind variables to values
          (sqlite3-bind-blob SQL_REF 1 (string->utf8 "hello-world"))
          (sqlite3-step SQL_REF)
          (let ((blob (sqlite3-column-blob SQL_REF 0)))
            (assert (bytevector=? blob #vu8(3 13 37))))

          (sqlite3-step SQL_END_TRANSACTION)
          (sqlite3-reset SQL_END_TRANSACTION)))

      (sqlite3-close sqlite3))))
