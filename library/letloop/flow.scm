#!chezscheme
(library (letloop flow)

  (export make-flow
          flow-abort
          flow-log
          flow-run
          flow-jiffy
          flow-parallel
          flow-sleep-jiffies
          flow-spawn
          flow-spawn-threadsafe
          flow-stop
          flow-tcp-serve

          flow-open
          flow-close
          flow-bytes
          flow-pread
          flow-pwrite
          flow-sync

          with-flow

          ~check-flow-000
          ~check-flow-001
          ~check-flow-002
          )

  (import (chezscheme)
          (scheme time)
          (srfi srfi-224)
          (letloop r999)
          (letloop epoll)
          (letloop cffi))

  ;;
  ;; inspired from https://stackoverflow.com/a/51777980/140837
  ;;
  ;; single thread, single event-loop
  ;;

  (define stdlib (load-shared-object #f))
  (define mutex (make-mutex))

  (define pk
    (lambda args
      (when (or (getenv "LETLOOP_FLOW")
                (getenv "LETLOOP_DEBUG"))
        (display "#;(letloop flow) " (current-error-port))
        (write args (current-error-port))
        (newline (current-error-port))
        (flush-output-port (current-error-port)))
      (car (reverse args))))

  (define flow-log
    (lambda (level message . objects)
      (pk level message objects)))

  (define flow-current (make-parameter #f))

  (define prompt-current #f)

  (define EWOULDBLOCK 11)
  (define EAGAIN EWOULDBLOCK)

  (define prompt-singleton '(prompt-singleton))

  (define-record-type* <flow>
    (make-flow-base start jiffy sleeping running epoll events thunks others readable writable)
    flow?
    (start flow-start)
    (jiffy flow-jiffy-base flow-jiffy!)
    (sleeping flow-sleeping flow-sleeping!)
    (running flow-running? flow-running!)
    (epoll flow-epoll)
    (events flow-events)
    (thunks flow-thunks flow-thunks!)
    (others flow-others flow-others!)
    (readable flow-readable)
    (writable flow-writable))

  (define call-with-flow-prompt
    (lambda (thunk handler)
      (call-with-values (lambda ()
                          (call/1cc
                           (lambda (k)
                             ;; XXX: The continuation K also called
                             ;; prompt-current may be called in THUNK during
                             ;; the extent of this lambda.
                             (set! prompt-current k)
                             (thunk))))
        (lambda out
          (cond
           ((and (pair? out) (eq? (car out) prompt-singleton))
            (apply handler (cdr out)))
           (else (apply values out)))))))

  (define flow-abort
    (lambda args
      (call/cc
       (lambda (k)
         ;; XXX: Capture the continuation and call it later, hence
         ;; call/cc instead of call/1cc.
         (let ((prompt prompt-current))
           (set! prompt-current #f)
           (apply prompt (cons prompt-singleton (cons k args))))))))

  (define make-event cons)
  (define event-continuation car)
  (define event-mode cdr)

  (define flow-apply
    (lambda (flow thunk)
      (guard (ex (else (pk ex) (pk (apply format #f
                                  (condition-message ex)
                                  (condition-irritants ex)))))
        (call-with-flow-prompt
         thunk
         (lambda (k handler)
           (handler k))))))

  (define hashtable-empty?
    (lambda (h)
      (fx=? (hashtable-size h) 0)))

  (define flow-jiffy
    (lambda ()
      (flow-jiffy-base (flow-current))))

  (define flow-run-once
    (lambda (flow)
      (define timeout #t)
      (flow-jiffy! flow (current-jiffy))
      (unless (fxmapping-empty? (flow-sleeping flow))
        (call-with-values (lambda () (fxmapping-split
                                      (flow-sleeping flow)
                                      (- (flow-jiffy)
                                         (flow-start (flow-current)))))
          (lambda (thunks sleeping)
            (flow-sleeping! flow sleeping)
            (for-each (lambda (thunk) (flow-apply flow thunk)) (fxmapping-values thunks)))))
      (let ((thunks (flow-thunks flow)))
        (flow-thunks! flow '())
        (for-each (lambda (thunk) (flow-apply flow thunk)) thunks))
      ;; this `unless` condition is dubious
      (when (and (not (hashtable-empty? (flow-events flow)))
                 (flow-running? flow))
        (if (fxmapping-empty? (flow-sleeping flow))
            (set! timeout -1)
            (set! timeout (floor
                           (/ (- (+ (flow-start (flow-current))
                                    (call-with-values (lambda ()
                                                        (fxmapping-min
                                                         (flow-sleeping flow)))
                                      (lambda args
                                        (car args))))
                                 (flow-jiffy))
                              (expt 10 6)))))
        ;; Wait for ONE event...
        (let* ((event (make-epoll-event))
               ;; TODO: increase max events from 1 to 1024?
               (count (epoll-wait (flow-epoll (flow-current)) event 1 timeout)))
          (if (fxzero? count)
              (foreign-free (ftype-pointer-address event))
              (let* ((mode (if (epoll-event-in? event) 'read 'write))
                     (k (hashtable-ref (flow-events flow)
                                       (cons (epoll-event-fd event) mode)
                                       #f)))
                (foreign-free (ftype-pointer-address event))
                (hashtable-delete! (flow-events flow) event)
                ;; remove the associated event mode from epoll instance
                (flow-apply flow k)))))))

  (define flow-watcher
    (lambda ()
      (flow-read (flow-readable (flow-current)))
      (let ((new
             (with-mutex mutex
               (let ((new (flow-others (flow-current))))
                 (flow-others! (flow-current) '())
                 new))))
        (flow-thunks! (flow-current)
                          (append new
                                  (flow-thunks
                                   (flow-current)))))
      (flow-watcher)))

  (define flow-stop
    (lambda ()
      (flow-running! (flow-current) #f)))

  (define flow-run
    (lambda ()
      (flow-spawn flow-watcher)
      (let loop ()
        (when (flow-running? (flow-current))
          (guard (ex (else (format #t "Exception ~a: ~a\n" (condition-message ex) (condition-irritants ex))))
            (flow-run-once (flow-current)))
          (loop)))
      (flow-current #f)))

  (define flow-spawn
    (lambda (thunk)
      (flow-thunks! (flow-current)
                        (cons thunk (flow-thunks (flow-current))))))

  (define flow-sleep-jiffies
    (lambda (nanoseconds)
      (flow-abort
       (lambda (k)
         (flow-sleeping! (flow-current)
                             (fxmapping-set (flow-sleeping (flow-current))
                                            (- (+ (flow-jiffy) nanoseconds)
                                               (flow-start (flow-current)))
                                            k))))))

  (define flow-spawn-threadsafe
    (lambda (thunk)
      (with-mutex mutex
        (flow-others! (flow-current)
                          (cons thunk (flow-others (flow-current)))))
      (flow-write (flow-writable (flow-current))
                      (bytevector 20 06))))

  (define flow-parallel
    (lambda (thunk)
      (flow-abort
       (lambda (k)
         (fork-thread (lambda () (call-with-values thunk
                                   (lambda args
                                     (flow-spawn-threadsafe (lambda () (apply k args)))))))))))

  (define fcntl!
    (let ((func (foreign-procedure "fcntl" (int int int) int)))
      (lambda (fd command value)
        (func fd command value))))

  (define fcntl
    (let ((func (foreign-procedure "fcntl" (int int) int)))
      (lambda (fd)
        (func fd F_GETFL))))

  (define-ftype <pipe>
    (array 2 int))

  (define F_GETFL 3)
  (define F_SETFL 4)
  (define O_NONBLOCK 2048)

  (define flow-nonblock!
    (lambda (fd)
      (fcntl! fd F_SETFL
              (fxlogior O_NONBLOCK
                        (fcntl fd)))))

  (define make-pipe
    (let ((func (foreign-procedure "pipe" (void* int) int)))
      (lambda ()
        (define pointer (foreign-alloc (ftype-sizeof <pipe>)))
        (call-with-errno (lambda () (func pointer 0))
          (lambda (out errno)
            (when (fx=? out -1)
              (error '(letloop flow) (strerror errno) errno))))
        (let  ((pipe (make-ftype-pointer <pipe> pointer)))
          (values (ftype-ref <pipe> (0) pipe) ;; readable
                  (ftype-ref <pipe> (1) pipe)))))) ;; writable

  (define make-flow
    (lambda ()
      (unless (flow-current)
        (flow-log 'notice "Making an flowment...")
        ;; (register-signal-handler 10
        ;;                          (lambda (oof)
        ;;                            (pk 'signaling... 'USR1?)))
        (call-with-values make-pipe
          (lambda (readable writable)
            (flow-nonblock! readable)
            (flow-nonblock! writable)
            (let ((epoll (epoll-create1 0))
                  (events (make-hashtable equal-hash equal?)))
              (flow-current (make-flow-base (current-jiffy)
                                                    (current-jiffy)
                                                    (fxmapping)
                                                    #t
                                                    epoll
                                                    events
                                                    '()
                                                    '()
                                                    readable
                                                    writable))))))
      (flow-current)))

  (define flow-socket
    (let ((socket (foreign-procedure "socket" (int int int) int)))
      (lambda (domain type protocol)
        (define out (socket domain type protocol))
        (flow-nonblock! out)
        out)))

  (define flow-accept-base
    (let ((flow-accept (foreign-procedure "accept4" (int void* void* int) int)))
      (lambda (fd)
        (flow-accept fd 0 0 2048))))

  (define flow-update-epoll
    (lambda (fd mode)
      (if (hashtable-ref (flow-events (flow-current))
                         (cons fd (if (eq? mode 'read) 'write 'read))
                         #f)
          (epoll-ctl (flow-epoll (flow-current))
                     3
                     fd
                     (make-epoll-event-out fd))
          (epoll-ctl (flow-epoll (flow-current))
                     2
                     fd
                     (make-epoll-event-out fd)))))


  (define flow-accept
    (lambda (fd)

      (define accept-handler
        (lambda (k)
          (hashtable-set! (flow-events (flow-current))
                          (cons fd 'read)
                          k)
          (epoll-ctl (flow-epoll (flow-current))
                     1
                     fd
                     (make-epoll-event-in fd))))

      (let loop ()
        (call-with-errno (lambda () (flow-accept-base fd))
          (lambda (out errno)
            (if (fx=? out -1)
                (if (fx=? errno EWOULDBLOCK)
                    (begin
                      (flow-abort accept-handler)
                      (flow-update-epoll fd 'read)
                      (loop))
                    #f)
                out))))))

  (define flow-close
    (let ((close (foreign-procedure "close" (int) int)))
      (lambda (fd)
        (close fd))))

  ;; taken from https://github.com/ecraven/chez-scheme-libraries/
  (define (setsockopt socket level optname optval)
    (define f (foreign-procedure "setsockopt" (int int int void* int) int))

    (define (bool opt-int)
      (let ((i (foreign-alloc (ftype-sizeof int)))
            (size (ftype-sizeof int)))
        (foreign-set! 'int i 0 (if optval 1 0))
        (call-with-errno (lambda () (f socket level opt-int i size))
          (lambda (out errno)
            (if (zero? out)
                #t
                (error 'setsockopt "Error on setsockopt" errno))))))

    (case optname
      ;; based on /usr/include/asm-generic/socket.h
      ((socket-option/debug) (bool 1))
      ((socket-option/reuseaddr) (bool 2))
      ((socket-option/dontroute) (bool 5))
      ((socket-option/broadcast) (bool 6))
      ;;((socket-option/sndbuf) (int 7))
      ;;((socket-option/rcvbuf) (int 8))
      ((socket-option/keepalive) (bool 9))
      ((socket-option/oobinline) (bool 10))
      ((socket-option/reuseport) (bool 15))
      ;;((socket-option/rcvlowat) (int 18))
      ;;((socket-option/sndlowat) (int 19))
      (else (error 'setsockopt "Unknown socket option" socket level optname optval))))

  (define flow-bind
    (let ((flow-bind
           (foreign-procedure "bind" (int void* size_t) int)))
      (lambda (fd ip port)
        (setsockopt fd 1 'socket-option/reuseaddr #t)
        (setsockopt fd 1 'socket-option/reuseport #t)
        (call-with-sockaddr-in
         ip port
         (lambda (address)
           (flow-bind fd
                          address
                          (ftype-sizeof %sockaddr-in)))))))

  (define-ftype %sockaddr-in
    (struct (family unsigned-short)
            (port (endian big unsigned-16))
            (address (endian big unsigned-32))
            (padding (array 8 char))))

  (define string->ipv4
    (lambda (string)

      (define (ipv4 one two three four)
        (+ (* one 256 256 256)
           (* two 256 256)
           (* three 256)
           four))

      (define make-char-predicate
        (lambda (char)
          (lambda (other)
            (char=? char other))))

      ;; taken from https://cookbook.scheme.org/split-string/
      (define (string-split char-delimiter? string)
        (define (maybe-add a b parts)
          (if (= a b) parts (cons (substring string a b) parts)))
        (let ((n (string-length string)))
          (let loop ((a 0) (b 0) (parts '()))
            (if (< b n)
                (if (not (char-delimiter? (string-ref string b)))
                    (loop a (+ b 1) parts)
                    (loop (+ b 1) (+ b 1) (maybe-add a b parts)))
                (reverse (maybe-add a b parts))))))

      (apply ipv4 (map string->number
                       (string-split (make-char-predicate #\.)
                                     string)))))

  (define (call-with-sockaddr-in address port proc)
    (let* ((ptr (foreign-alloc (ftype-sizeof %sockaddr-in)))
           (res (make-ftype-pointer %sockaddr-in ptr)))
      ;; create socket FAMILY=inet
      (ftype-set! %sockaddr-in (family) res 2)
      (ftype-set! %sockaddr-in (port) res port)
      (ftype-set! %sockaddr-in (address) res (string->ipv4 address))
      (call-with-values (lambda () (proc (ftype-pointer-address res)))
        (lambda args
          (foreign-free ptr)
          (apply values args)))))

  (define flow-listen
    (let ((flow-listen (foreign-procedure "listen" (int int) int)))
      (lambda (fd backlog)
        (flow-listen fd backlog))))

  (define flow-read-base
    (let ((flow-read
           (foreign-procedure "read" (int void* size_t) ssize_t)))
      (lambda (fd bytevector)
        (with-lock (list bytevector)
                   (flow-read fd
                                  (bytevector-pointer bytevector)
                                  (bytevector-length bytevector))))))

  (define flow-read
    (lambda (fd)
      (define bv (make-bytevector 1024))

      (define handler
        (lambda (k)
          (hashtable-set! (flow-events (flow-current))
                          (cons fd 'read)
                          k)
          (epoll-ctl (flow-epoll (flow-current))
                     1
                     fd
                     (make-epoll-event-in fd))))

      (let loop ()
        (call-with-errno (lambda () (flow-read-base fd bv))
          (lambda (out errno)
            (if (fx=? out -1)
                (if (fx=? errno EWOULDBLOCK)
                    (begin
                      (flow-abort handler)
                      (loop))
                    #f)
                (if (fxzero? out)
                    #f
                    (subbytevector bv 0 out))))))))


  (define flow-write-base
    (let ((flow-write
           (foreign-procedure "write" (int void* size_t) ssize_t)))
      (lambda (fd bytevector)
        (with-lock (list bytevector)
                   (flow-write fd
                                   (bytevector-pointer bytevector)
                                   (bytevector-length bytevector))))))

  (define flow-write
    (lambda (fd bv)
      (define handler
        (lambda (k)
          (hashtable-set! (flow-events (flow-current))
                          (cons fd 'write)
                          k)
          (epoll-ctl (flow-epoll (flow-current))
                     3 ;; EPOLL_CTL_MOD
                     fd
                     (make-epoll-event-out fd))))

      (let loop ((bv bv))
        (call-with-errno (lambda () (flow-write-base fd bv))
          (lambda (out errno)
            (if (fx=? out -1)
                (if (fx=? errno EWOULDBLOCK)
                    (begin
                      (flow-abort handler)
                      (loop bv))
                    #f)
                (if (fx=? out (bytevector-length bv))
                    #t
                    (loop (subbytevector bv out (bytevector-length bv))))))))))

  (define subbytevector
    (case-lambda
      ((bv start end)
       (assert (bytevector? bv))
       (unless (<= 0 start end (bytevector-length bv))
         (error 'subbytevector "Invalid indices: ~a ~a ~a" bv start end))
       (if (and (fxzero? start)
                (fx=? end (bytevector-length bv)))
           bv
           (let ((ret (make-bytevector (fx- end start))))
             (bytevector-copy! bv start
                               ret 0 (fx- end start))
             ret)))
      ((bv start)
       (subbytevector bv start (bytevector-length bv)))))

  (define flow-tcp-serve
    (lambda (ip port)
      (define SOCKET-DOMAIN=AF-INET 2)
      (define SOCKET-TYPE=STREAM 1)
      (define fd (flow-socket SOCKET-DOMAIN=AF-INET SOCKET-TYPE=STREAM 0))

      (define close (lambda () (flow-close fd)))

      (define read
        (lambda ()
          (let ((client (flow-accept fd)))
            (if client
                (values (lambda () (flow-read client))
                        (lambda (bv) (flow-write client bv))
                        (lambda () (flow-close client)))
                (begin
                  (values #f #f #f))))))


      (flow-bind fd ip port)
      (flow-listen fd 128)

      (values read close)))

  (define flow-open
    (let ((open (foreign-procedure "open" (string int integer-32) int)))
      (lambda (path options)

        (define file-options->flags
          (lambda (o)
            (if (pair? o)
                (apply fxlogior (map file-options->flags o))
                (case o
                  (flow-file-append 1024)
                  (flow-file-create 64)
                  (flow-file-direct 65536)
                  (flow-file-read-only 0)
                  (flow-file-write-only 1)
                  (flow-file-read-write 2)
                  (else (error 'letloop-flow "Unknown flow-file option" o))))))

        (call-with-values
            (lambda ()
              (flow-parallel
               (lambda ()
                 (call-with-errno (lambda () (open path
                                                   (file-options->flags options)
                                                   ;; what is 128, what is 256
                                                   (fxlogior 128 256)))
                   values))))
          (lambda (out errno)
            (if (fx=? out -1)
                (error 'letloop-flow "Failed to open file: ~a ~a ~a" path options (strerror errno))
                out))))))

  (define flow-bytes
    (let ((lseek (foreign-procedure "lseek" (int unsigned-64 int) unsigned-64)))
      (lambda (fd)
        (define SEEK_END 2)
        (call-with-values (lambda ()
                            (flow-parallel
                             (lambda ()
                               (call-with-errno (lambda () (lseek fd 0 SEEK_END)) values))))
          (lambda (out errno)
            ;; otherwise out is the absolute offset from the beginning of the file
            (if (= out (- (expt 2 64) 1))
                (error 'letloop-flow "Failed to seek to the end of file descriptor: ~a ~a" fd (strerror errno))
                out))))))

  (define flow-sync
    (let ((sync (foreign-procedure "sync_file_range" (int unsigned-64 unsigned-64 unsigned-32) int)))
      (lambda (fd offset length)
        ;; The magic 7 is: SYNC_FILE_RANGE_WAIT_BEFORE | SYNC_FILE_RANGE_WRITE | SYNC_FILE_RANGE_WAIT_AFTER
        (flow-parallel (lambda () (sync fd offset length 7))))))

  (define bytevector-slice
    (lambda (bv start end)
      (unless (<= 0 start end (bytevector-length bv))
        (error 'bytevector-slice "Invalid index start, or end" start end))
      (let ((ret (make-bytevector (fx- end start))))
        (bytevector-copy! bv start
                          ret 0 (fx- end start))
        ret)))

  (define flow-pread
    (let ((pread (foreign-procedure "pread" (int void* size_t unsigned-64) ssize_t)))
      (lambda (fd offset length)
        (define buffer (make-bytevector length))
        (call-with-values (lambda ()
                            (flow-parallel
                             (lambda ()
                               (call-with-errno (lambda () (with-lock (list buffer)
                                                             (pread fd
                                                                    (bytevector-pointer buffer)
                                                                    length
                                                                    offset)))
                                 values))))
          (lambda (out errno)
            (if (fx=? out -1)
                (error 'letloop-flow "Failed to pread: ~a ~a" fd (strerror errno))
                (if (fx=? out length)
                    buffer
                    (bytevector-slice buffer 0 out))))))))

  (define flow-pwrite
    (let ((pwrite (foreign-procedure "pwrite" (int void* size_t unsigned-64) ssize_t)))
      (lambda (fd offset bytevector)
        (let loop ((bytevector bytevector))
          (call-with-values (lambda ()
                              (flow-parallel
                               (lambda ()
                                 (call-with-errno (lambda () (with-lock (list bytevector)
                                                               (pwrite fd
                                                                       (bytevector-pointer bytevector)
                                                                       (bytevector-length bytevector)
                                                                       offset)))
                                   values))))
            (lambda (out errno)
              (if (fx=? out -1)
                  (error 'letloop-flow "Failed to pwrite: ~a ~a" fd (strerror errno))
                  (if (fx=? out 0)
                      (error 'letloop-flow "Failed to pwrite, nothing was written" fd)
                      (unless (fx=? out (bytevector-length bytevector))
                        (loop ((bytevector-slice bytevector out (bytevector-length bytevector)))))))))))))


  (define-syntax with-jiffies
    (syntax-rules ()
      ((with-jiffies body ...)
       (let ((start (current-jiffy)))
         body ...
         (- (current-jiffy) start)))))

  (define ~check-flow-000
    (lambda ()
      (< 3 (with-jiffies
            (make-flow)
            (flow-spawn (lambda ()
                              (flow-sleep-jiffies (* 4 (expt 10 9)))
                              (flow-stop)))
            (flow-run)))))

  (define fib
    (lambda (n)
      (cond
       ((= n 0) 0)
       ((= n 1) 1)
       (else (+ (fib (- n 1))
                (fib (- n 2)))))))

  (define-syntax with-flow
    (syntax-rules ()
      ((with-flow body ...)
       (let ((flow (make-flow)))
         (flow-spawn (lambda () body ... (flow-stop)))
         (flow-run)
         #t))))

  (define ~check-flow-001
    (lambda ()
      ;; check that flow-parallel let other lambda to run in the
      ;; main thread.
      (> (let ()
          (define inc 0)
          (define a #f)
          (define b #f)
          (make-flow)
          (flow-spawn
           (lambda ()
             (let loop ()
               (set! inc (+ inc 1))
               (flow-sleep-jiffies (expt 10 3))
               (loop))))
          (flow-spawn
           (lambda ()
             (set! a (flow-parallel (lambda () (fib 21))))
             (set! b (flow-parallel (lambda () (fib 21))))
             (flow-stop)))
          (flow-run)
          inc)
         (let ()
           (define inc 0)
           (define a #f)
           (define b #f)
           (make-flow)
           (flow-spawn
            (lambda ()
              (let loop ()
                (set! inc (+ inc 1))
                (flow-sleep-jiffies (expt 10 6))
                (loop))))
           (flow-spawn
            (lambda ()
              (set! a (fib 21))
              (set! b (fib 21))
              (flow-stop)))
           (flow-run)
           inc))))

  (define ~check-flow-002
    (lambda ()
      (define expected (bytevector 1 2 3 4 5 6 7 8 9 10 11 12 13))
      (make-flow)
      (flow-spawn
       (lambda ()
         (define fd (flow-open "check-flow-002-0" (list 'flow-file-create 'flow-file-read-write)))
         (assert (= 0 (flow-bytes fd)))
         (assert (= 0 (bytevector-length (flow-pread fd 0 13))))
         (flow-pwrite fd 0 expected)
         (flow-sync fd 0 13)
         (assert (= 13 (flow-bytes fd)))
         (assert (equal? expected (flow-pread fd 0 13)))
         (flow-stop)))
      (flow-run)
      #t))

  )
