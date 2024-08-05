(import (chezscheme)
        (only (scheme process-context) get-environment-variables get-environment-variable)
        (only (scheme list) list-index)
        (letloop root))

;; helpers

(define pk
  (lambda args
    (when (get-environment-variable "DEBUG_JUST")
      (display ";; " (current-error-port))
      (write args (current-error-port))
      (newline (current-error-port))
      (flush-output-port (current-error-port)))
    (car (reverse args))))

(define stdlib (load-shared-object #f))

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

(define basename
  (lambda (string)
    (let loop ((index (string-length string)))
      (if (char=? (string-ref string (- index 1)) #\/)
          (substring string index (string-length string))
          (loop (- index 1))))))

(define without-extension
  (lambda (string)
    (let loop ((index (string-length string)))
      (if (zero? index)
          string
          (if (char=? (string-ref string (- index 1)) #\.)
              (substring string 0 (- index 1))
              (loop (- index 1)))))))

;; just-linux

(define just-linux
  (let ((directory (root-temporary-directory "/tmp/letloop/just-linux")))
    (root-init-exec "debian" "bookworm" "amd64" directory)
    directory))

;; (define just-linux "/tmp/letloop/just-linux-Yt5yh2/")
(define src "/mnt/just-linux/seed/src/")

(define e0 (root-exec-exec just-linux #f '() "apt update --yes"))
(define e1 (root-exec-exec just-linux #f '() "apt install --yes build-essential git vim curl wget gawk bash bison python3 patchelf texinfo"))
(define e2 (root-exec-exec just-linux #f '() "mkdir -pv /mnt/just-linux/seed/src/"))



(define linux `((name . "linux")
                (version . "6.10.2")
                (source . "https://www.kernel.org/pub/linux/kernel/v6.x/linux-6.10.2.tar.xz")
                (build .
                       ,(lambda (directory)
                          (root-exec-exec just-linux directory '() "make mrproper")
                          (root-exec-exec just-linux directory '() "make headers")
                          (root-exec-exec just-linux directory '() "find usr/include -type f ! -name '*.h' -delete")
                          (root-exec-exec just-linux directory '() "cp -rv usr/include /mnt/just-linux/seed/")))))

(define extension
  (lambda (string)
    (let loop ((index (string-length string)))
      (if (zero? index)
          string
          (if (char=? (string-ref string (- index 1)) #\.)
              (substring string index (string-length string))
              (loop (- index 1)))))))

(define (string-split delimiter? string)
  (let ((result (list))
        (output (open-output-string)))
    (string-for-each
     (lambda (char)
       (if (delimiter? char)
           (begin
             (set! result (cons (get-output-string output) result))
             (set! output (open-output-string)))
           (write-char char output)))
     string)
    (reverse (cons (get-output-string output) result))))

(define patchelf
  (lambda (directory)

    (define read-string
      (lambda (filename)
        (call-with-input-file filename
          (lambda (p)
            (let loop ((chars '()))
              (let ((char (read-char p)))
                (if (eof-object? char)
                    (list->string (reverse chars))
                    (loop (cons char chars)))))))))

    (define files (append

                   (map (lambda (x) (cons x (string-append "/just/seed/lib/" x)))
                        (filter (lambda (x) (or (string=? "so" (extension x))
                                                (string=? "so" (extension (without-extension x)))))
                                (directory-list (string-append directory "/just/seed/lib/"))))

                   (guard (ex (else '()))
                     (map (lambda (x) (cons x (string-append "/just/seed/bin/" x)))
                          (directory-list (string-append directory "/just/seed/bin/"))))

                   (map (lambda (x) (cons x (string-append "/just/seed/sbin/" x)))
                        (directory-list (string-append directory "/just/seed/sbin/")))

                   (map (lambda (x) (cons x (string-append "/just/seed/usr/bin/" x)))
                        (directory-list (string-append directory "/just/seed/usr/bin/")))))

    (define patchelf-needed-symbol
      (lambda (file needed)
        (unless (char=? (string-ref needed 0) #\/)
          (guard (ex (else #f))
            (system* #f #f "patchelf --replace-needed ~a ~a ~a/~a" needed (cdr (assoc needed files)) "/home/amirouche/rootfs/" file)))))

    (define patchelf-file
      (lambda (file)
        (let ((needed (guard (ex (else '()))
                        (system* #f #f "patchelf --print-needed ~a/~a > /tmp/output 2> /dev/null" "/home/amirouche/rootfs/" file)
                        (filter (lambda (x) (not (string=? x "")))
                                (string-split (lambda (x) (char=? #\newline x))
                                              (read-string "/tmp/output"))))))
          (for-each (lambda (x) (patchelf-needed-symbol file x)) needed))
        (guard (ex (else '()))
          (system* #f #f "patchelf --print-interpreter ~a/~a > /tmp/output 2> /dev/null" "/home/amirouche/rootfs/" file)
          (let ((interpreter (string-split (lambda (x) (char=? #\newline x))
                                           (read-string "/tmp/output"))))
            (system* #f #f "patchelf --set-interpreter ~a ~a/~a" (cdr (assoc (basename (car interpreter)) files)) "/home/amirouche/rootfs/" file)))))

    (for-each patchelf-file (map cdr files))))

(define glibc `((name . "glibc")
                (version . "2.40")
                (source . "https://ftp.gnu.org/gnu/glibc/glibc-2.40.tar.xz")
                (build .
                       ,(lambda (directory)
                          (root-exec-exec just-linux directory '()
                                          "wget https://www.linuxfromscratch.org/patches/lfs/development/glibc-2.40-fhs-1.patch")
                          (root-exec-exec just-linux directory '() "patch -Np1 -i glibc-2.40-fhs-1.patch")
                          (root-exec-exec just-linux directory '() "mkdir -p build")
                          (root-exec-exec just-linux (string-append directory "/build") '()
                                          (string-append "../configure"
                                                         " --prefix=/usr/"
                                                         " --enable-kernel=4.19"
                                                         " --with-headers=/mnt/just-linux/seed/include/"
                                                         " --disable-nscd"
                                                         " libc_cv_slibdir=/lib/"))
                          (root-exec-exec just-linux (string-append directory "/build") '()
                                          "make -j$(nproc --ignore=1)")
                          (root-exec-exec just-linux (string-append directory "/build") '()
                                          "make DESTDIR=/mnt/just-linux/seed/ install")
                          (patchelf (string-append just-linux "/mnt/just-linux/seed/"))))))

(define gcc `((name . "gcc")
                (version . "14.2.0")
                (source . "https://ftp.gnu.org/gnu/gcc/gcc-14.2.0/gcc-14.2.0.tar.xz")
                (build .
                       ,(lambda (directory)
                          (root-exec-exec just-linux directory '()
                                          "wget https://ftp.gnu.org/gnu/mpfr/mpfr-4.2.1.tar.xz")
                          (root-exec-exec just-linux directory '()
                                          "tar xf mpfr-4.2.1.tar.xz")
                          (root-exec-exec just-linux directory '()
                                          "mv mpfr-4.2.1 mpfr")
                          (root-exec-exec just-linux directory '()
                                          "wget https://ftp.gnu.org/gnu/mpc/mpc-1.3.1.tar.gz")
                          (root-exec-exec just-linux directory '()
                                          "tar xf mpc-1.3.1.tar.gz")
                          (root-exec-exec just-linux directory '()
                                          "mv mpc-1.3.1 mpc")
                          (root-exec-exec just-linux directory '()
                                          "wget https://ftp.gnu.org/gnu/gmp/gmp-6.3.0.tar.xz")
                          (root-exec-exec just-linux directory '()
                                          "tar xf gmp-6.3.0.tar.xz")
                          (root-exec-exec just-linux directory '()
                                          "mv gmp-6.3.0 gmp")
                          (root-exec-exec just-linux directory '()
                                          "sed -e '/m64=/s/lib64/lib/' -i.orig gcc/config/i386/t-linux64")
                          (root-exec-exec just-linux directory '() "mkdir -p build")
                          (root-exec-exec just-linux (string-append directory "/build") '()
                                          (string-append "../configure"
                                                         " --prefix=/usr/"
                                                         " --with-glibc-version=2.40"
                                                         " --with-sysroot=/"
                                                         " --with-newlib"
                                                         " --without-headers"
                                                         " --enable-default-pie"
                                                         " --enable-default-ssp"
                                                         " --disable-nls"
                                                         " --disable-shared"
                                                         " --disable-multilib"
                                                         " --disable-threads"
                                                         " --disable-libatomic"
                                                         " --disable-bootstrap"
                                                         " --disable-libgomp"
                                                         " --disable-libquadmath"
                                                         " --disable-libssp"
                                                         " --disable-libvtv"
                                                         " --enable-languages=c,c++"))
                          (root-exec-exec just-linux (string-append directory "/build") '()
                                          "make -j$(nproc --ignore=1)")
                          (root-exec-exec just-linux (string-append directory "/build") '()
                                          "make DESTDIR=/mnt/just-linux/seed/ install")
                          (patchelf (string-append just-linux "/mnt/just-linux/seed/"))))))


(define binutils `((name . "binutils")
                   (version . "2.42")
                   (source . "https://sourceware.org/pub/binutils/releases/binutils-2.42.tar.xz")
                   (build .
                          ,(lambda (directory)
                             (root-exec-exec just-linux directory '() "mkdir -p build")
                             (root-exec-exec just-linux (string-append directory "/build") '()
                                             (string-append "../configure"
                                                            " --prefix=/usr"
                                                            " --with-sysroot=/"
                                                            " --disable-nls"
                                                            " --enable-gprofng=no"
                                                            " --disable-werror"
                                                            " --enable-new-dtags"
                                                            " --enable-default-hash-style=gnu"))
                             (root-exec-exec just-linux (string-append directory "/build") '()
                                             "make -j$(nproc --ignore=1)")
                             (root-exec-exec just-linux (string-append directory "/build") '()
                                             "make DESTDIR=/mnt/just-linux/seed/ install")))))

(define m4 `((name . "m4")
             (version . "2.42")
             (source . "https://ftp.gnu.org/gnu/m4/m4-1.4.19.tar.xz")
             (build .
                    ,(lambda (directory)
                       (root-exec-exec just-linux directory '() "mkdir -p build")
                       (root-exec-exec just-linux (string-append directory "/build") '()
                                       (string-append "../configure"
                                                      " --prefix=/usr"))
                       (root-exec-exec just-linux (string-append directory "/build") '()
                                       "make -j$(nproc --ignore=1)")
                       (root-exec-exec just-linux (string-append directory "/build") '()
                                       "make DESTDIR=/mnt/just-linux/seed/ install")))))

(define ncurses
  `((name . "ncurses")
    (version . "6.4-...")
    (source . "https://invisible-mirror.net/archives/ncurses/ncurses-6.5.tar.gz")
    (build .
           ,(lambda (directory)
              (root-exec-exec just-linux directory '() "sed -i s/mawk// configure")
              (root-exec-exec just-linux directory '() "mkdir -p build")
              (root-exec-exec just-linux (string-append directory "/build") '()
                              (string-append "../configure"))
              (root-exec-exec just-linux (string-append directory "/build") '()
                              "make -j$(nproc --ignore=1) -C include")
              (root-exec-exec just-linux (string-append directory "/build") '()
                              "make -j$(nproc --ignore=1) -C progs tic")
              (root-exec-exec just-linux directory '()
                              "./configure --prefix=/usr --with-shared --without-normal --with-cxx-shared --without-debug --without-ada --disable-stripping")
              (root-exec-exec just-linux directory '()
                              "make")
              (root-exec-exec just-linux directory '()
                              "make DESTDIR=/mnt/just-linux/seed/ TIC_PATH=$(pwd)/build/progs/tic install")
              (root-exec-exec just-linux directory '()
                              "ln -sv libncursesw.so /mnt/just-linux/seed/usr/lib/libncurses.so")
              (root-exec-exec just-linux directory '()
                              "sed -e 's/^#if.*XOPEN.*$/#if 1/' -i /mnt/just-linux/seed//usr/include/curses.h")))))

(define steps (list
               linux
               glibc
               gcc
               binutils
               m4
               ncurses))

(for-each (lambda (step)
            (define work (string-append src "/" (cdr (assq 'name step))))
            (root-exec-exec just-linux #f '() "mkdir -p ~s" work)
            (root-exec-exec just-linux work '() "wget ~s" (cdr (assq 'source step)))
            (root-exec-exec just-linux work '() "tar xf ~s" (basename (cdr (assq 'source step))))
            ((cdr (assq 'build step)) (string-append work "/"
                                                     (without-extension
                                                      (without-extension
                                                       (basename (cdr (assq 'source step))))))))
          steps)

(patchelf "/home/amirouche/rootfs/")
(define ei1 (root-exec-exec just-linux #f '() "bash"))
