(import (chezscheme))
(import (only (scheme process-context) get-environment-variables get-environment-variable))
(import (only (scheme list) list-index))
(import (letloop www))
(import (letloop html))
(import (letloop sxpath))
(import (scheme generator))

;; helpers

(define pk
  (lambda args
    (when (get-environment-variable "DEBUG_ROOT")
      (display ";; " (current-error-port))
      (write args (current-error-port))
      (newline (current-error-port)))
    (car (reverse args))))

(define stdlib (load-shared-object #f))

(define (make-temporary-directory prefix)

  (define mkdtemp
    (foreign-procedure "mkdtemp" (string) string))

  (system* #f #f "mkdir -p $(dirname ~s)" prefix)
  
  (let ((input (string-append prefix "-XXXXXX")))
    (pk (mkdtemp input))))

(define call-with-env
  (lambda (env thunk)

    (define unsetenv
      (let ((func (foreign-procedure "unsetenv" (string) int)))
        (lambda (string)
          (func string))))

    ;; backup variables before overriding
    (define original (get-environment-variables))

    (if (not env)
        (thunk)
        ;; override!
        (begin
          (let loop ((env env))
            (unless (null? env)
              (putenv (symbol->string (caar env)) (cdar env))
              (loop (cdr env))))
          ;; call thunk
          (call-with-values thunk
            (lambda args
              (let loop ((env env))
                (if (null? env)
                    ;; bring back original variables
                    (let loop ((original original))
                      (unless (null? original)
                        (putenv (caar original) (cdar original))
                        (loop (cdr original))))
                    (begin
                      ;; unset variables from ENV
                      (unsetenv (symbol->string (caar env)))
                      (loop (cdr env)))))
              (apply values args)))))))

(define system?
  (lambda (command)
    (zero? (system command))))

(define system*
  (lambda (directory env command . variables)
    (pk 'system* directory env command variables)
    ;; TODO: check directory exists
    (unless (or (not directory)
                (and 
                 (file-exists? directory)
                 (file-directory? directory)))
      (error 'root "directory not found" directory))
    (unless (call-with-env env (lambda ()
                                 (if directory
                                     (parameterize ((current-directory directory))
                                       (system? (apply format #f command variables)))
                                     (system? (apply format #f command variables)))))
      (error 'run "failed" directory env command))))

(define (command-line-parse arguments)

  ;; Given the following ARGUMENTS:
  ;;
  ;;   '("--foo=bar" "--qux" "-vvv" "name" "another" "--" "olive" "extra")
  ;;
  ;; command-line-parse* returns the following values:
  ;;
  ;;   (values '((--foo . "bar") (--qux . #t) (-vvv . #t))
  ;;           '("name" "other")
  ;;           '("olive" "extra"))
  ;;
  ;; Standalone arguments e.g. "name" and "other" and extra arguments
  ;; e.g. "olive" and "extra" are returned in the same order as
  ;; found in ARGUMENTS.

  (define keyword/value
    (lambda (string)
      (define index (list-index (lambda (x) (char=? x #\=)) (string->list string)))


      (if (not index)
          (values (string->symbol string) #t)
          (values (string->symbol (substring string 0 index)) (substring string (+ index 1) (string-length string))))))

  (let loop ((arguments arguments)
             (keywords '())
             (standalone '()))
    (if (null? arguments)
        (values keywords (reverse standalone) '())
        (let ((head (car arguments)))
          (cond
           ((string=? head "--")
            (values keywords (reverse standalone) (cdr arguments)))
           ((char=? (string-ref head 0) #\-)
            (call-with-values (lambda () (keyword/value head))
              (lambda (key value)
                (loop (cdr arguments) (cons (cons key value) keywords) standalone))))
           (else (loop (cdr arguments) keywords (cons head standalone))))))))


(define IMAGES.LINUXCONTAINERS.ORG "https://images.linuxcontainers.org/images/")

;; template url
(define url_rootfs "{URL}{distribution}/{release}/{arch}/default/{build}/rootfs.tar.xz")

#!chezscheme
(define sxpath-index-distributions
  (sxpath '(// a @ href *text*)))

(define root-index-hrefs
  (lambda (url)
    (call-with-values (lambda () (www-request 'GET url '() (bytevector)))
      (lambda (code headers body)
        (if (= code 200)
            ;; With cdr, remove ../
            (cdr (sxpath-index-distributions (html-read (utf8->string body))))
            '())))))

(define root-distribution-hrefs
  (lambda ()
    (root-index-hrefs IMAGES.LINUXCONTAINERS.ORG)))

(define root-distribution-version-hrefs
  (lambda (distribution-href)
    (root-index-hrefs (string-append IMAGES.LINUXCONTAINERS.ORG distribution-href))))

(define root-distribution-version-machine-hrefs
  (lambda (distribution-href version-href)
    (root-index-hrefs (string-append IMAGES.LINUXCONTAINERS.ORG distribution-href version-href))))

(define root-distribution-version-machine-latest-build
  (lambda (distribution-href version-href machine-href)

    (define and=>
      (lambda (x p)
        (if (null? x) x (p x))))
    
    (and=> (reverse (root-index-hrefs (string-append IMAGES.LINUXCONTAINERS.ORG
                                                     ;; there might be duplicated slash
                                                     distribution-href "/"
                                                     version-href "/"
                                                     machine-href "/"
                                                     "default/")))
           car)))

(define root-available-generator
  (lambda ()

    (define rstrip/
      (lambda (x)
        (substring x 0 (- (string-length x) 1))))
    
    (make-coroutine-generator
     (lambda (yield)
       (for-each
        (lambda (distribution)
          (for-each
           (lambda (version)
             (for-each
              (lambda (machine)
                (yield (list (rstrip/ distribution)
                             (rstrip/ version)
                             (rstrip/ machine))))
              (root-distribution-version-machine-hrefs distribution version)))
           (root-distribution-version-hrefs distribution)))
        (root-distribution-hrefs))))))
         
(define root-available
  (lambda ()
    (generator-for-each (lambda (x) (apply format #t "~a ~a ~a\n" x)) (root-available-generator))))

(define basename
  (lambda (string)
    (let loop ((index (string-length string)))
      (if (char=? (string-ref string (- index 1)) #\/)
          (substring string index (string-length string))
          (loop (- index 1))))))

(define root-init-exec
  (lambda (distribution version machine directory)

    (define build (root-distribution-version-machine-latest-build distribution
                                                                  version
                                                                  machine))
    (define rootfs.tar.xz (string-append IMAGES.LINUXCONTAINERS.ORG
                                         distribution "/"
                                         version "/"
                                         machine "/"
                                         "default" "/"
                                         build
                                         "rootfs.tar.xz"))
    (define SHA256SUMS (string-append IMAGES.LINUXCONTAINERS.ORG
                                      distribution "/"
                                      version "/"
                                      machine "/"
                                      "default" "/"
                                      build
                                      "SHA256SUMS"))

    (and (system* directory '() "wget ~a" rootfs.tar.xz)
         (system* directory '() "wget ~a" SHA256SUMS)
         (system* directory '() "fgrep rootfs.tar.xz SHA256SUMS | sha256sum -c -")
         (system* directory '() "tar xf rootfs.tar.xz")
         ;; TODO: why rm machine-id
         (system* directory '() "rm -f etc/resolv.conf etc/machine-id")
         (system* directory '() "echo ~a > etc/hostname" (basename directory))
         (system* directory '() "echo root filesystem available @ ~a" directory))))

(define root-init
  (lambda ()
    (error 'root "not implemented")))

(define root-exec-exec
  (lambda (directory target-directory env command)
    (define string-join
      (lambda (strings delimiter)
        (let loop ((out (list delimiter))
                   (strings strings))
          (if (null? strings)
              (apply string-append (reverse out))
              (loop (cons* delimiter (car strings) out)
                    (cdr strings))))))

    (define env* (if (not env) ""
                     (string-join (map (lambda (x) (string-append " " (symbol->string (car x))
                                                                  "=" (cdr x)))
                                       env)
                                  " ")))
    (define target-directory* (or target-directory "/"))

    (system* directory #f "cp /etc/resolv.conf ~a/etc/resolv.conf" directory)
    (system* directory #f "mkdir -p ~a/mnt/host" directory)
    (system* #f
             #f
             (string-append "sudo systemd-nspawn --uuid=$(systemd-id128 new) --directory=~s"
                            " --bind=$(pwd):/mnt/host --chdir=~s"
                            " --machine=~a"
                            " --private-users=pick"
                            " /usr/bin/env ~a"
                            " ~a")
             directory
             target-directory*
             (basename directory)
             env*
             command)))

(define root-spawn-exec
  (lambda (directory)
    (system* #f
             #f
             (string-append "sudo systemd-nspawn --uuid=$(systemd-id128 new) --directory=~s"
                            " --boot"
                            " --capability=CAP_NET_ADMIN"
                            " --bind=$(pwd):/mnt/host"
                            " --private-users=pick"
                            " --machine=~a")
             directory
             (basename directory))))
    

;; (define tmp (make-temporary-directory "/tmp/letloop-root/bookbook"))
;; (root-init-exec "debian" "bookworm" "amd64" tmp)
;; (root-exec-exec tmp "/tmp" '((TOTO . "TITI")) "/bin/bash")
;; (root-exec-exec "/tmp/letloop-root/bookbook-6Xwngr" "/tmp" '() "/bin/bash")
(root-spawn-exec "/tmp/letloop-root/bookbook-6Xwngr")
