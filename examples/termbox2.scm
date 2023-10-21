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
    (termbox-print 3 "   hello world!" TB-GREEN)
    (termbox-print 5 message TB-WHITE)
    (tb-present)
    (let ((event (dg 'poll (tb-poll-event))))
      (dg 'event event)
      (if (key? event)
          (begin
            (dg 'key (key->alist event))
            (when (and (equal? (key-key event) #\q)
                       (key-ctrl? event))
              (set! continue? #f)))))
    (set! message (string-append "You typed something... " (number->string count)))
    (if continue?
        (loop (fx+ count 1))
        (tb-shutdown))))

(main)
