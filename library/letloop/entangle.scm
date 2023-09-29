#!chezscheme
(library (letloop entangle)

  (export make-entangle
          entangle-abort
          entangle-log
          entangle-run
          entangle-jiffy
          entangle-sleep-jiffies
          entangle-spawn
          entangle-spawn-threadsafe
          entangle-stop
          entangle-tcp-serve
          ~check-entangle-000
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
      (when (or (getenv "LETLOOP_ENTANGLE")
                (getenv "LETLOOP_DEBUG"))
        (display "#;(letloop entangle) " (current-error-port))
        (write args (current-error-port))
        (newline (current-error-port))
        (flush-output-port (current-error-port)))
      (car (reverse args))))

  (define entangle-log
    (lambda (level message . objects)
      (pk level message objects)))

  (define entangle-current (make-parameter #f))

  (define prompt-current #f)

  (define EWOULDBLOCK 11)
  (define EAGAIN EWOULDBLOCK)

  (define prompt-singleton '(prompt-singleton))

  (define-record-type* <entangle>
    (make-entangle-base start jiffy sleeping running epoll events thunks others readable writable)
    entangle?
    (start entangle-start)
    (jiffy entangle-jiffy-base entangle-jiffy!)
    (sleeping entangle-sleeping entangle-sleeping!)
    (running entangle-running? entangle-running!)
    (epoll entangle-epoll)
    (events entangle-events)
    (thunks entangle-thunks entangle-thunks!)
    (others entangle-others entangle-others!)
    (readable entangle-readable)
    (writable entangle-writable))

  (define call-with-entangle-prompt
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

  (define entangle-abort
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

  (define entangle-apply
    (lambda (entangle thunk)
      (guard (ex (else (pk (apply format #f
                                  (condition-message ex)
                                  (condition-irritants ex)))))
        (call-with-entangle-prompt
         thunk
         (lambda (k handler)
           (handler k))))))

  (define hashtable-empty?
    (lambda (h)
      (fx=? (hashtable-size h) 0)))

  (define entangle-jiffy
    (lambda ()
      (entangle-jiffy-base (entangle-current))))

  (define entangle-run-once
    (lambda (entangle)
      (define timeout #t)
      (entangle-jiffy! entangle (current-jiffy))
      (unless (fxmapping-empty? (entangle-sleeping entangle))
        (call-with-values (lambda () (fxmapping-split
                                      (entangle-sleeping entangle)
                                      (- (entangle-jiffy)
                                         (entangle-start (entangle-current)))))
          (lambda (thunks sleeping)
            (entangle-sleeping! entangle sleeping)
            (for-each (lambda (thunk) (entangle-apply entangle thunk)) (fxmapping-values thunks)))))
      (entangle-thunks entangle)
      (for-each (lambda (thunk) (entangle-apply entangle thunk)) (entangle-thunks entangle))
      (entangle-thunks! entangle '())
      ;; this `unless` condition is dubious
      (when (and (not (hashtable-empty? (entangle-events entangle)))
                 (entangle-running? entangle))
        (if (fxmapping-empty? (entangle-sleeping entangle))
            (set! timeout -1)
            (set! timeout (floor
                           (/ (- (+ (entangle-start (entangle-current))
                                    (call-with-values (lambda ()
                                                        (fxmapping-min
                                                         (entangle-sleeping entangle)))
                                      (lambda args
                                        (car args))))
                                 (entangle-jiffy))
                              (expt 10 6)))))
        ;; Wait for ONE event...
        (let* ((event (make-epoll-event))
               ;; TODO: increase max events from 1 to 1024?
               (count (epoll-wait (entangle-epoll (entangle-current)) event 1 timeout)))
          (if (fxzero? count)
              (foreign-free (ftype-pointer-address event))
              (let* ((mode (if (epoll-event-in? event) 'read 'write))
                     (k (hashtable-ref (entangle-events entangle)
                                       (cons (epoll-event-fd event) mode)
                                       #f)))
                (foreign-free (ftype-pointer-address event))
                (hashtable-delete! (entangle-events entangle) event)
                ;; remove the associated event mode from epoll instance
                (entangle-apply entangle k)))))))

  (define entangle-watcher
    (lambda ()
      (entangle-read (entangle-readable (entangle-current)))
      (let ((new
             (with-mutex mutex
               (let ((new (entangle-others (entangle-current))))
                 (entangle-others! (entangle-current) '())
                 new))))
        (entangle-thunks! (entangle-current)
                          (append new
                                  (entangle-thunks
                                   (entangle-current)))))
      (entangle-watcher)))

  (define entangle-stop
    (lambda ()
      (entangle-running! (entangle-current) #f)))

  (define entangle-run
    (lambda ()
      (entangle-spawn entangle-watcher)
      (let loop ()
        (when (entangle-running? (entangle-current))
          (guard (ex (else (format #t "Exception ~a: ~a\n" (condition-message ex) (condition-irritants ex))))
            (entangle-run-once (entangle-current)))
          (loop)))))

  (define entangle-spawn
    (lambda (thunk)
      (entangle-thunks! (entangle-current)
                        (cons thunk (entangle-thunks (entangle-current))))))

  (define entangle-sleep-jiffies
    (lambda (nanoseconds)
      (entangle-abort
       (lambda (k)
         (entangle-sleeping! (entangle-current)
                             (fxmapping-set (entangle-sleeping (entangle-current))
                                            (- (+ (entangle-jiffy) nanoseconds)
                                               (entangle-start (entangle-current)))
                                            k))))))

  (define entangle-spawn-threadsafe
    (lambda (thunk)
      (with-mutex mutex
        (entangle-others! (entangle-current)
                          (cons thunk (entangle-others (entangle-current)))))
      (entangle-write (entangle-writable (entangle-current))
                      (bytevector 20 06))))

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

  (define entangle-nonblock!
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
              (error '(letloop entangle) (strerror errno) errno))))
        (let  ((pipe (make-ftype-pointer <pipe> pointer)))
          (values (ftype-ref <pipe> (0) pipe) ;; readable
                  (ftype-ref <pipe> (1) pipe)))))) ;; writable

  (define make-entangle
    (lambda ()
      (unless (entangle-current)
        (entangle-log 'notice "Making an entanglement...")
        ;; (register-signal-handler 10
        ;;                          (lambda (oof)
        ;;                            (pk 'signaling... 'USR1?)))
        (call-with-values make-pipe
          (lambda (readable writable)
            (entangle-nonblock! readable)
            (entangle-nonblock! writable)
            (let ((epoll (epoll-create1 0))
                  (events (make-hashtable equal-hash equal?)))
              (entangle-current (make-entangle-base (current-jiffy)
                                                    (current-jiffy)
                                                    (fxmapping)
                                                    #t
                                                    epoll
                                                    events
                                                    '()
                                                    '()
                                                    readable
                                                    writable))))))
      (entangle-current)))

  (define entangle-socket
    (let ((socket (foreign-procedure "socket" (int int int) int)))
      (lambda (domain type protocol)
        (define out (socket domain type protocol))
        (entangle-nonblock! out)
        out)))

  (define entangle-accept-base
    (let ((entangle-accept (foreign-procedure "accept4" (int void* void* int) int)))
      (lambda (fd)
        (entangle-accept fd 0 0 2048))))

  (define entangle-update-epoll
    (lambda (fd mode)
      (if (hashtable-ref (entangle-events (entangle-current))
                         (cons fd (if (eq? mode 'read) 'write 'read))
                         #f)
          (epoll-ctl (entangle-epoll (entangle-current))
                     3
                     fd
                     (make-epoll-event-out fd))
          (epoll-ctl (entangle-epoll (entangle-current))
                     2
                     fd
                     (make-epoll-event-out fd)))))


  (define entangle-accept
    (lambda (fd)

      (define accept-handler
        (lambda (k)
          (hashtable-set! (entangle-events (entangle-current))
                          (cons fd 'read)
                          k)
          (epoll-ctl (entangle-epoll (entangle-current))
                     1
                     fd
                     (make-epoll-event-in fd))))

      (let loop ()
        (call-with-errno (lambda () (entangle-accept-base fd))
          (lambda (out errno)
            (if (fx=? out -1)
                (if (fx=? errno EWOULDBLOCK)
                    (begin
                      (entangle-abort accept-handler)
                      (entangle-update-epoll fd 'read)
                      (loop))
                    #f)
                out))))))

  (define entangle-close
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

  (define entangle-bind
    (let ((entangle-bind
           (foreign-procedure "bind" (int void* size_t) int)))
      (lambda (fd ip port)
        (setsockopt fd 1 'socket-option/reuseaddr #t)
        (setsockopt fd 1 'socket-option/reuseport #t)
        (call-with-sockaddr-in
         ip port
         (lambda (address)
           (entangle-bind fd
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

  (define entangle-listen
    (let ((entangle-listen (foreign-procedure "listen" (int int) int)))
      (lambda (fd backlog)
        (entangle-listen fd backlog))))

  (define entangle-read-base
    (let ((entangle-read
           (foreign-procedure "read" (int void* size_t) ssize_t)))
      (lambda (fd bytevector)
        (with-lock (list bytevector)
                   (entangle-read fd
                                  (bytevector-pointer bytevector)
                                  (bytevector-length bytevector))))))

  (define entangle-read
    (lambda (fd)
      (define bv (make-bytevector 1024))

      (define handler
        (lambda (k)
          (hashtable-set! (entangle-events (entangle-current))
                          (cons fd 'read)
                          k)
          (epoll-ctl (entangle-epoll (entangle-current))
                     1
                     fd
                     (make-epoll-event-in fd))))

      (let loop ()
        (call-with-errno (lambda () (entangle-read-base fd bv))
          (lambda (out errno)
            (if (fx=? out -1)
                (if (fx=? errno EWOULDBLOCK)
                    (begin
                      (entangle-abort handler)
                      (loop))
                    #f)
                (if (fxzero? out)
                    #f
                    (subbytevector bv 0 out))))))))


  (define entangle-write-base
    (let ((entangle-write
           (foreign-procedure "write" (int void* size_t) ssize_t)))
      (lambda (fd bytevector)
        (with-lock (list bytevector)
                   (entangle-write fd
                                   (bytevector-pointer bytevector)
                                   (bytevector-length bytevector))))))

  (define entangle-write
    (lambda (fd bv)
      (define handler
        (lambda (k)
          (hashtable-set! (entangle-events (entangle-current))
                          (cons fd 'write)
                          k)
          (epoll-ctl (entangle-epoll (entangle-current))
                     3 ;; EPOLL_CTL_MOD
                     fd
                     (make-epoll-event-out fd))))

      (let loop ((bv bv))
        (call-with-errno (lambda () (entangle-write-base fd bv))
          (lambda (out errno)
            (if (fx=? out -1)
                (if (fx=? errno EWOULDBLOCK)
                    (begin
                      (entangle-abort handler)
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

  (define entangle-tcp-serve
    (lambda (ip port)
      (define SOCKET-DOMAIN=AF-INET 2)
      (define SOCKET-TYPE=STREAM 1)
      (define fd (entangle-socket SOCKET-DOMAIN=AF-INET SOCKET-TYPE=STREAM 0))

      (define close (lambda () (entangle-close fd)))

      (define read
        (lambda ()
          (let ((client (entangle-accept fd)))
            (if client
                (values (lambda () (entangle-read client))
                        (lambda (bv) (entangle-write client bv))
                        (lambda () (entangle-close client)))
                (begin
                  (values #f #f #f))))))


      (entangle-bind fd ip port)
      (entangle-listen fd 128)

      (values read close)))

  (define ~check-entangle-000
    (lambda ()
      (make-entangle)
      (entangle-spawn (lambda ()
                        (entangle-sleep-jiffies (* 9 (expt 10 9)))
                        (entangle-stop)))
      (entangle-run)
      #t))

  )
