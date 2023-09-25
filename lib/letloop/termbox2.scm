(library (letloop termbox2)
  (export
   tb-init
   tb-shutdown
   tb-change-cell
   tb-present
   tb-clear
   tb-height
   tb-width
   tb-set-cursor
   tb-hide-cursor
   tb-poll-event
   tb-select-output-mode

   tb-event-type
   tb-event-char
   tb-event-key-mod
   tb-event-key-shift?
   tb-event-key-alt?
   tb-event-key-ctrl?
   tb-event-key-key
   tb-event-resize-width
   tb-event-resize-height
   tb-event-mouse-mod
   tb-event-mouse-key
   tb-event-mouse-x
   tb-event-mouse-y

   key?
   key-ctrl?
   key-alt?
   key-shift?
   key-key
   key->alist

   TB-DEFAULT
   TB-BLACK
   TB-RED
   TB-GREEN
   TB-YELLOW
   TB-BLUE
   TB-MAGENTA
   TB-CYAN
   TB-WHITE

   TB-BOLD
   TB-UNDERLINE
   TB-REVERSE

   TB-OUTPUT-NORMAL
   TB-OUTPUT-256
   TB-OUTPUT-216
   TB-OUTPUT-GRAYSCALE
   TB-OUTPUT-TRUECOLOR

   TB-EVENT-KEY
   TB-EVENT-RESIZE
   TB-EVENT-MOUSE

   TB-MOD-ALT
   TB-MOD-CTRL
   TB-MOD-SHIFT
   TB-MOD-MOTION

   TB-EUNSUPPORTED-TERMINAL
   TB-EFAILED-TO-OPEN-TTY
   TB-EPIPE-TRAP-ERROR)
  (import
   (chezscheme)
   (letloop r999))

  ;; TODO: extract foreign-procedure* outside lambda or define

  (begin

    (define-syntax define-syntax-rule
      (syntax-rules ()
        ((define-syntax-rule (keyword args ...) body)
         (define-syntax keyword
           (syntax-rules ()
             ((keyword args ...) body))))))

    (define libtermbox (load-shared-object "./local/lib/libtermbox2.so"))

    ;; ffi helpers

    (define-syntax-rule (pointer->ftype struct pointer)
      (make-ftype-pointer struct (foreign-ref 'void* pointer 0)))

    (define-syntax-rule (foreign-procedure* return ptr args ...)
      (foreign-procedure ptr (args ...) return))

    ;; bindings

    (define TB-KEY-F1 (- #xFFFF 0))
    (define TB-KEY-F2 (- #xFFFF 1))
    (define TB-KEY-F3 (- #xFFFF 2))
    (define TB-KEY-F4 (- #xFFFF 3))
    (define TB-KEY-F5 (- #xFFFF 4))
    (define TB-KEY-F6 (- #xFFFF 5))
    (define TB-KEY-F7 (- #xFFFF 6))
    (define TB-KEY-F8 (- #xFFFF 7))
    (define TB-KEY-F9 (- #xFFFF 8))
    (define TB-KEY-F10 (- #xFFFF 9))
    (define TB-KEY-F11 (- #xFFFF 10))
    (define TB-KEY-F12 (- #xFFFF 11))
    (define TB-KEY-INSERT (- #xFFFF 12))
    (define TB-KEY-DELETE (- #xFFFF 13))
    (define TB-KEY-HOME (- #xFFFF 14))
    (define TB-KEY-END (- #xFFFF 15))
    (define TB-KEY-PGUP (- #xFFFF 16))
    (define TB-KEY-PGDN (- #xFFFF 17))
    (define TB-KEY-ARROW-UP (- #xFFFF 18))
    (define TB-KEY-ARROW-DOWN (- #xFFFF 19))
    (define TB-KEY-ARROW-LEFT (- #xFFFF 20))
    (define TB-KEY-ARROW-RIGHT (- #xFFFF 21))
    (define TB-KEY-MOUSE-LEFT (- #xFFFF 22))
    (define TB-KEY-MOUSE-RIGHT (- #xFFFF 23))
    (define TB-KEY-MOUSE-MIDDLE (- #xFFFF 24))
    (define TB-KEY-MOUSE-RELEASE (- #xFFFF 25))
    (define TB-KEY-MOUSE-WHEEL-UP (- #xFFFF 26))
    (define TB-KEY-MOUSE-WHEEL-DOWN (- #xFFFF 27))

    (define TB-KEY-CTRL-TILDE #x00)
    (define TB-KEY-CTRL-2 #x00) ;; clash with CTRL-TILDE
    (define TB-KEY-CTRL-A #x01)
    (define TB-KEY-CTRL-B #x02)
    (define TB-KEY-CTRL-C #x03)
    (define TB-KEY-CTRL-D #x04)
    (define TB-KEY-CTRL-E #x05)
    (define TB-KEY-CTRL-F #x06)
    (define TB-KEY-CTRL-G #x07)
    (define TB-KEY-BACKSPACE #x08)
    (define TB-KEY-CTRL-H #x08) ;; clash with CTRL-BACKSPACE
    (define TB-KEY-TAB #x09)
    (define TB-KEY-CTRL-I #x09) ;; clash with TAB
    (define TB-KEY-CTRL-J #x0A)
    (define TB-KEY-CTRL-K #x0B)
    (define TB-KEY-CTRL-L #x0C)
    (define TB-KEY-ENTER #x0D)
    (define TB-KEY-CTRL-M #x0D) ;; clash with ENTER
    (define TB-KEY-CTRL-N #x0E)
    (define TB-KEY-CTRL-O #x0F)
    (define TB-KEY-CTRL-P #x10)
    (define TB-KEY-CTRL-Q #x11)
    (define TB-KEY-CTRL-R #x12)
    (define TB-KEY-CTRL-S #x13)
    (define TB-KEY-CTRL-T #x14)
    (define TB-KEY-CTRL-U #x15)
    (define TB-KEY-CTRL-V #x16)
    (define TB-KEY-CTRL-W #x17)
    (define TB-KEY-CTRL-X #x18)
    (define TB-KEY-CTRL-Y #x19)
    (define TB-KEY-CTRL-Z #x1A)
    (define TB-KEY-ESC #x1B)
    (define TB-KEY-CTRL-LSQ-BRACKET #x1B) ;; class with ESC
    (define TB-KEY-CTRL-3 #x1B) ;; clash with ESC
    (define TB-KEY-CTRL-4 #x1C)
    (define TB-KEY-CTRL-BACKSLASH #x1C) ;; clash with CTRL-4
    (define TB-KEY-CTRL-5 #x1D)
    (define TB-KEY-CTRL-RSQ-BRACKET #x1D) ;; clash with CTRL-5
    (define TB-KEY-CTRL-6 #x1E)
    (define TB-KEY-CTRL-7 #x1F)
    (define TB-KEY-CTRL-SLASH #x1F) ;; clash with CTRL-7
    (define TB-KEY-CTRL-UNDERSCORE #x1F) ;; clash with CTRL-7
    (define TB-KEY-SPACE #x20)
    (define TB-KEY-BACKSPACE2 #x7F)
    (define TB-KEY-CTRL-8 #x7F) ;; clash with BACKSPACE2

    (define TB-MOD-ALT #x01)
    (define TB-MOD-CTRL #x02)
    (define TB-MOD-SHIFT #x04)
    (define TB-MOD-MOTION #x08)

    (define TB-DEFAULT #x00)
    (define TB-BLACK #x01)
    (define TB-RED #x02)
    (define TB-GREEN #x03)
    (define TB-YELLOW #x04)
    (define TB-BLUE #x05)
    (define TB-MAGENTA #x06)
    (define TB-CYAN #x07)
    (define TB-WHITE #x08)

    (define TB-BOLD #x01000000)
    (define TB-UNDERLINE #x02000000)
    (define TB-REVERSE #x04000000)

    (define TB-EVENT-KEY 1)
    (define TB-EVENT-RESIZE 2)
    (define TB-EVENT-MOUSE 3)

    (define TB-EUNSUPPORTED-TERMINAL -1)
    (define TB-EFAILED-TO-OPEN-TTY -2)
    (define TB-EPIPE-TRAP-ERROR -3)

    (define-syntax switch
      (syntax-rules ()
        [(switch key (value out) ... (else expr))
         (cond
          ((= key value) out)
          ...
          (else expr))]))

    (define-record-type* <key>
      (make-key ctrl? alt? shift? key)
      key?
      (ctrl? key-ctrl?)
      (alt? key-alt?)
      (shift? key-shift?)
      (key key-key))

    (define key->alist
      (lambda (x)
        `((ctrl? ,(key-ctrl? x))
          (alt? ,(key-alt? x))
          (shift? ,(key-shift? x))
          (key ,(key-key x)))))

    (define (char-event->key mode key)
      (let ((CTRL? (fx= (fxlogand mode TB-MOD-CTRL) TB-MOD-CTRL))
            (ALT? (fx= (fxlogand mode TB-MOD-ALT) TB-MOD-ALT))
            (SHIFT? (fx= (fxlogand mode TB-MOD-SHIFT) TB-MOD-SHIFT)))
        (switch key
                (TB-KEY-F1 (make-key CTRL? ALT? SHIFT? 'F1))
                (TB-KEY-F2 (make-key CTRL? ALT? SHIFT? 'F2))
                (TB-KEY-F3 (make-key CTRL? ALT? SHIFT? 'F3))
                (TB-KEY-F4 (make-key CTRL? ALT? SHIFT? 'F4))
                (TB-KEY-F5 (make-key CTRL? ALT? SHIFT? 'F5))
                (TB-KEY-F6 (make-key CTRL? ALT? SHIFT? 'F6))
                (TB-KEY-F7 (make-key CTRL? ALT? SHIFT? 'F7))
                (TB-KEY-F8 (make-key CTRL? ALT? SHIFT? 'F8))
                (TB-KEY-F9 (make-key CTRL? ALT? SHIFT? 'F9))
                (TB-KEY-F10 (make-key CTRL? ALT? SHIFT? 'F10))
                (TB-KEY-F11 (make-key CTRL? ALT? SHIFT? 'F11))
                (TB-KEY-F12 (make-key CTRL? ALT? SHIFT? 'F12))
                (TB-KEY-INSERT (make-key CTRL? ALT? SHIFT? 'insert))
                (TB-KEY-DELETE (make-key CTRL? ALT? SHIFT? 'delete))
                (TB-KEY-HOME (make-key CTRL? ALT? SHIFT? 'home))
                (TB-KEY-END (make-key CTRL? ALT? SHIFT? 'end))
                (TB-KEY-PGUP (make-key CTRL? ALT? SHIFT? 'page-up))
                (TB-KEY-PGDN (make-key CTRL? ALT? SHIFT? 'page-down))
                (TB-KEY-ARROW-UP (make-key CTRL? ALT? SHIFT? 'arrow-up))
                (TB-KEY-ARROW-DOWN (make-key CTRL? ALT? SHIFT? 'arrow-down))
                (TB-KEY-ARROW-LEFT (make-key CTRL? ALT? SHIFT? 'arrow-left))
                (TB-KEY-ARROW-RIGHT (make-key CTRL? ALT? SHIFT? 'arrow-right))
                (TB-KEY-CTRL-TILDE (make-key #t ALT? SHIFT? #\~))
                (TB-KEY-CTRL-2 (make-key #t ALT? SHIFT? #\2))
                (TB-KEY-CTRL-A (make-key #t ALT? SHIFT? #\a))
                (TB-KEY-CTRL-B (make-key #t ALT? SHIFT? #\b))
                (TB-KEY-CTRL-C (make-key #t ALT? SHIFT? #\c))
                (TB-KEY-CTRL-D (make-key #t ALT? SHIFT? #\d))
                (TB-KEY-CTRL-E (make-key #t ALT? SHIFT? #\e))
                (TB-KEY-CTRL-F (make-key #t ALT? SHIFT? #\f))
                (TB-KEY-CTRL-G (make-key #t ALT? SHIFT? #\g))
                (TB-KEY-BACKSPACE (make-key #t ALT? SHIFT? 'backspace))
                (TB-KEY-CTRL-H (make-key #t ALT? SHIFT? #\h))
                (TB-KEY-TAB (make-key #t ALT? SHIFT? 'tab))
                (TB-KEY-CTRL-I (make-key #t ALT? SHIFT? #\i))
                (TB-KEY-CTRL-J (make-key #t ALT? SHIFT? #\j))
                (TB-KEY-CTRL-K (make-key #t ALT? SHIFT? #\k))
                (TB-KEY-CTRL-L (make-key #t ALT? SHIFT? #\l))
                (TB-KEY-ENTER (make-key CTRL? ALT? SHIFT? 'enter))
                (TB-KEY-CTRL-M (make-key #t ALT? SHIFT? #\m))
                (TB-KEY-CTRL-N (make-key #t ALT? SHIFT? #\n))
                (TB-KEY-CTRL-O (make-key #t ALT? SHIFT? #\o))
                (TB-KEY-CTRL-P (make-key #t ALT? SHIFT? #\p))
                (TB-KEY-CTRL-Q (make-key #t ALT? SHIFT? #\q))
                (TB-KEY-CTRL-R (make-key #t ALT? SHIFT? #\r))
                (TB-KEY-CTRL-S (make-key #t ALT? SHIFT? #\s))
                (TB-KEY-CTRL-T (make-key #t ALT? SHIFT? #\t))
                (TB-KEY-CTRL-U (make-key #t ALT? SHIFT? #\u))
                (TB-KEY-CTRL-V (make-key #t ALT? SHIFT? #\v))
                (TB-KEY-CTRL-W (make-key #t ALT? SHIFT? #\w))
                (TB-KEY-CTRL-X (make-key #t ALT? SHIFT? #\x))
                (TB-KEY-CTRL-Y (make-key #t ALT? SHIFT? #\y))
                (TB-KEY-CTRL-Z (make-key #t ALT? SHIFT? #\z))
                (TB-KEY-ESC (make-key #t ALT? SHIFT? 'escape))
                ;; not sure what it is and clash with TB-KEY-ESC
                ;; (TB-KEY-CTRL-LSQ-BRACKET (make-key #t ALT? SHIFT? 'F1))
                ;; clash with TB-KEY-CTRL-ESC
                ;; (TB-KEY-CTRL-3 (make-key #t ALT? SHIFT? '3))
                (TB-KEY-CTRL-4 (make-key #t ALT? SHIFT? #\4))
                ;; clash with TB-KEY-CTRL-BACKSLASH
                ;; (TB-KEY-CTRL-BACKSLASH (make-key #t ALT? SHIFT? 'F1))
                (TB-KEY-CTRL-5 (make-key #t ALT? SHIFT? #\5))
                ;; not sure what it is and clash with TB-KEY-CTRL-5
                ;; (TB-KEY-CTRL-RSQ-BRACKET (make-key #t ALT? SHIFT? 'F1))
                (TB-KEY-CTRL-6 (make-key #t ALT? SHIFT? #\6))
                (TB-KEY-CTRL-7 (make-key #t ALT? SHIFT? #\7))
                ;; not sure what it is and class with TB-KEY-CTRL-7
                ;; (TB-KEY-CTRL-SLASH (make-key #t ALT? SHIFT? 'F1))
                ;; (TB-KEY-CTRL-UNDERSCORE (make-key #t ALT? SHIFT? 'F1))
                (TB-KEY-SPACE (make-key CTRL? ALT? SHIFT? #\space))
                ;; not sure what it is but defined in termbox
                (TB-KEY-BACKSPACE2 (make-key #t ALT? SHIFT? 'backspace2))
                ;; clash with TB-KEY-BACKSPACE2
                ;; (TB-KEY-CTRL-8 (make-key #t ALT? SHIFT? #\8))
                (else #f))))

    (define tb-init
      (let ((proc (foreign-procedure* int "tb_init")))
        (lambda ()
          (proc))))

    (define tb-shutdown
      (let ((proc (foreign-procedure* void "tb_shutdown")))
        (lambda ()
          (proc))))

    (define tb-width
      (let ((proc (foreign-procedure* int "tb_width")))
        (lambda ()
          (proc))))

    (define tb-height
      (let ((proc (foreign-procedure* int "tb_height")))
        (lambda ()
          (proc))))

    (define tb-clear
      (let ((proc (foreign-procedure* int "tb_clear")))
        (lambda ()
          (proc))))

    #;(define tb-set-clear-attributes
      (let ((proc (foreign-procedure* void "tb_set_clear_attributes" unsigned-32 unsigned-32)))
        (lambda (fg bg)
          (proc fg bg))))

    (define tb-present
      (let ((proc (foreign-procedure* void "tb_present")))
        (lambda ()
          (proc))))

    (define tb-set-cursor
      (let ((proc (foreign-procedure* void "tb_set_cursor" int int)))
        (lambda (x y)
          (proc x y))))

    (define tb-hide-cursor
      (let ((proc (foreign-procedure* void "tb_hide_cursor")))
        (lambda ()
          (proc))))

    (define tb-change-cell
      (let ((proc (foreign-procedure* void
                                      "tb_set_cell"
                                      int
                                      int
                                      unsigned-32
                                      unsigned-32
                                      unsigned-32)))
        (lambda  (x y ch fg bg)
          (proc x y ch fg bg))))

    (define TB-OUTPUT-CURRENT 0)
    (define TB-OUTPUT-NORMAL 1)
    (define TB-OUTPUT-256 2)
    (define TB-OUTPUT-216 3)
    (define TB-OUTPUT-GRAYSCALE 4)
    (define TB-OUTPUT-TRUECOLOR 5)

    (define tb-select-output-mode
      (let ((proc (foreign-procedure* void "tb_set_output_mode" int)))
        (lambda (mode)
          (proc mode))))

    ;; event

    (define-ftype tb-event
      (struct
       [type integer-8]
       [mod integer-8]
       [key unsigned-16]
       [ch unsigned-32]
       [w integer-32]
       [h integer-32]
       [x integer-32]
       [y integer-32]))

    (define-record-type* <event>
      (%make-event type a b c d)
      event?
      (type tb-event-type)
      (a event-a)
      (b event-b)
      (c event-c)
      (d event-d))

    ;; TODO: assume (from srfi-145) on event-type
    (define tb-event-char event-a)

    (define tb-event-key-mod event-a)
    (define tb-event-key-key event-b)

    (define tb-event-key-alt?
      (lambda (x)
        (fx=? (fxlogand (tb-event-key-mod x) TB-MOD-ALT) TB-MOD-ALT)))

    (define tb-event-key-shift?
      (lambda (x)
        (fx=? (fxlogand (tb-event-key-mod x) TB-MOD-SHIFT) TB-MOD-SHIFT)))

    (define tb-event-key-ctrl?
      (lambda (x)
        (fx=? (fxlogand (tb-event-key-mod x) TB-MOD-CTRL) TB-MOD-CTRL)))

    (define tb-event-resize-width event-a)
    (define tb-event-resize-height event-b)

    (define tb-event-mouse-mod event-a)
    (define tb-event-mouse-key event-b)
    (define tb-event-mouse-x event-c)
    (define tb-event-mouse-y event-d)

    (define (make-event event)
      (define type (ftype-ref tb-event (type) event))
      (case type
        ((-1) (error 'termbox "event error"))
        ;; TODO: what is this?
        ((0) #f)
        ;; char event
        ((1) (if (= (ftype-ref tb-event (ch) event) 0)
                 (let* ((mode (ftype-ref tb-event (mod) event))
                        (key (char-event->key mode (ftype-ref tb-event (key) event))))
                   key)
                 (make-key #f #f #f (integer->char (ftype-ref tb-event (ch) event)))))
        ;; resize event
        ((2) (%make-event 'resize
                          (ftype-ref tb-event (w) event)
                          (ftype-ref tb-event (h) event)
                          #f
                          #f))
        ;; mouse event
        ((3) (%make-event 'mouse
                          (ftype-ref tb-event (mod) event)
                          (ftype-ref tb-event (key) event)
                          (ftype-ref tb-event (x) event)
                          (ftype-ref tb-event (y) event)))
        (else (error 'termbox "Termbox oops!" type))))

    (define (make-tb-event)
      (make-ftype-pointer tb-event (foreign-alloc (ftype-sizeof tb-event))))

    (define tb-peek-event
      (let ((proc (foreign-procedure* int "tb_peek_event" void* int)))
        (lambda (timeout)
          (let ((event (make-tb-event)))
            (proc (ftype-pointer-address event) timeout)
            (make-event event)))))

    (define tb-poll-event
      (let ((proc (foreign-procedure* int "tb_poll_event" void*)))
        (lambda ()
          (let ((event (make-tb-event)))
            (proc (ftype-pointer-address event))
            (make-event event)))))

    ))
