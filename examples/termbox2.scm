(import (chezscheme))
(import (letloop termbox2))


(define termbox-print
  (lambda (line string color)
    (let loop ((index 0)
               (chars (string->list string)))
      (unless (null? chars)
        (tb-change-cell index line (char->integer (car chars)) color TB-DEFAULT)
        (loop (fx+ index 1) (cdr chars))))))

(define message "   Type any key on the keyboard. To quit, use CTRL+Q.")
(define continue? #t)

(define %error (open-file-output-port "out.log"
                                      (file-options no-fail)
                                      (buffer-mode line)
                                      (native-transcoder)))

(define (dg . rest)
  (write rest %error) (newline %error)
  (car (reverse rest)))

(define (main)
  (tb-init)
  ;; (tb-select-output-mode TB-INIT)
  (let loop ((count 0))
    (tb-clear)
    (tb-hide-cursor)
    (termbox-print 3 "   Welcome to termbox typer!" TB-GREEN)
    (termbox-print 5 message TB-WHITE)
    (tb-present)
    (let ((event (tb-poll-event)))
      (when (and (key? event)
                 (equal? (key-key event) #\q)
                 (key-ctrl? event))
        (set! continue? #f))
      (when (key? event)
        (set! message "   You pressed key: ")
        (when (key-ctrl? event)
          (set! message (string-append message "control ")))
        (when (key-alt? event)
          (set! message (string-append message "alt ")))
        (when (key-shift? event)
          (set! message (string-append message "shift ")))
        (if (symbol? (key-key event))
            (set! message (string-append message (symbol->string (key-key event))))
            (set! message (string-append message (list->string (list (key-key event))))))
        (termbox-print 7 message TB-BLUE)))
    (if continue?
        (loop (fx+ count 1))
        (tb-shutdown))))

(main)

