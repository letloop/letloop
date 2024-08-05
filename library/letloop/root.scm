(library (letloop root)

  (export letloop-root
          root-temporary-directory
          root-available-print
          root-init
          root-exec
          root-spawn
          root-emulate)
   
   (import (chezscheme)
           (only (scheme process-context) get-environment-variables get-environment-variable)
           (only (scheme list) list-index)
           (letloop www)
           (letloop html)
           (letloop sxpath)
           (scheme generator))
   

   ;; helpers

   (define pk
     (lambda args
       (when (get-environment-variable "DEBUG_ROOT")
         (display ";; " (current-error-port))
         (write args (current-error-port))
         (newline (current-error-port)))
       (car (reverse args))))

   (define stdlib (load-shared-object #f))

   (define root-temporary-directory
     (lambda (prefix)

       (define mkdtemp
         (foreign-procedure "mkdtemp" (string) string))

       (system* #f #f "mkdir -p $(dirname ~s)" prefix)
       
       (let ((input (string-append prefix "-XXXXXX")))
         (mkdtemp input))))

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
         (error 'system* "non-zero exit code" directory env command variables))))

   (define URL_IMAGES_INDEX "https://images.linuxcontainers.org/images/")

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
       (root-index-hrefs URL_IMAGES_INDEX)))

   (define root-distribution-version-hrefs
     (lambda (distribution-href)
       (root-index-hrefs (string-append URL_IMAGES_INDEX distribution-href))))

   (define root-distribution-version-machine-hrefs
     (lambda (distribution-href version-href)
       (root-index-hrefs (string-append URL_IMAGES_INDEX distribution-href version-href))))

   (define root-distribution-version-machine-latest-build
     (lambda (distribution-href version-href machine-href)

       (define and=>
         (lambda (x p)
           (if (null? x) x (p x))))
       
       (and=> (reverse (root-index-hrefs (string-append URL_IMAGES_INDEX
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
   
   (define root-available-print
     (lambda ()
       (generator-for-each (lambda (x) (apply format #t "~a ~a ~a\n" x)) (root-available-generator))))

   (define basename
     (lambda (string)
       (let loop ((index (string-length string)))
         (if (char=? (string-ref string (- index 1)) #\/)
             (substring string index (string-length string))
             (loop (- index 1))))))

   (define root-init
     (lambda (distribution version machine directory)

       (define build (root-distribution-version-machine-latest-build distribution
                                                                     version
                                                                     machine))
       (define rootfs.tar.xz (string-append URL_IMAGES_INDEX
                                            distribution "/"
                                            version "/"
                                            machine "/"
                                            "default" "/"
                                            build
                                            "rootfs.tar.xz"))
       (define SHA256SUMS (string-append URL_IMAGES_INDEX
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
   
   (define string-join
     (lambda (strings delimiter)
       (let loop ((out (list delimiter))
                  (strings strings))
         (if (null? strings)
             (apply string-append (reverse out))
             (loop (cons* delimiter (car strings) out)
                   (cdr strings))))))

   (define root-exec
     (lambda (directory target-directory env command . variables)

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
                (string-append "systemd-nspawn --uuid=$(systemd-id128 new) --directory=~s"
                               " --bind=$(pwd):/mnt/host --chdir=~s"
                               " --machine=~a"
                               " /usr/bin/env ~a"
                               " ~a")
                directory
                target-directory*
                (basename directory)
                env*
                (apply format #f command variables))))

   (define root-spawn
     (lambda (directory)
       (system* #f
                #f
                (string-append "systemd-nspawn --uuid=$(systemd-id128 new) --directory=~s"
                               " --boot"
                               " --capability=CAP_NET_ADMIN"
                               " --bind=$(pwd):/mnt/host"
                               " --machine=~a")
                directory
                (basename directory))))

   (define qemu-9p-bare-command "qemu-system-x86_64 \
    -enable-kvm \
    -machine pc,accel=kvm,usb=off,dump-guest-core=off -m 2048 \
    -smp 4,sockets=4,cores=1,threads=1 -rtc base=utc \
    -boot strict=on -kernel ~a \
    -initrd ~a \
    -append 'init=/usr/lib/systemd/systemd root=fsRoot rw rootfstype=9p rootflags=trans=virtio,version=9p2000.L,msize=5000000,posixacl console=ttyS0' \
    -fsdev local,security_model=none,multidevs=remap,id=fsRoot,path=~a \
    -device virtio-9p-pci,id=fsRoot,fsdev=fsRoot,mount_tag=fsRoot \
    -nographic")

   (define root-emulate
     (lambda (directory)
       ;; TODO: support more machine architecture amd64, aarch64, etc...
       ;;
       ;; Install linux-image-amd64, add fsRoot /etc/fstab, and include 9p in initrd
       ;;
       ;; ref: https://superuser.com/a/536352/115319
       (system* #f #f qemu-9p-bare-command
                (string-append directory "/boot/vmlinuz*")
                (string-append directory "/boot/initrd.img*")
                directory)))
   
   ;; (define tmp (make-temporary-directory "/tmp/letloop-root/bookbook"))
   ;; (root-init "debian" "bookworm" "amd64" tmp)
   ;; (root-exec tmp #f #f "/bin/bash")
   ;; (root-exec "/tmp/letloop-root/bookbook-8TPZrc/" "/tmp" '() "/bin/bash")
   ;; (root-spawn "/tmp/letloop-root/bookbook-6Xwngr")
   ;; (root-emulate "/tmp/letloop-root/bookbook-8TPZrc/")

   (define letloop-root
     (lambda (args)
       (if (null? args)
           (begin (display "You can do it!\n")
                  (exit 1))
           (case (string->symbol (car args))
             ((available) (root-available-print))
             ((init) (apply root-init (cdr args)))
             ((exec) (root-exec (cadr args) (caddr args) '() (string-join (cddr (cddr args)) " ")))
             ((spawn) (root-spawn (cadr args)))
             ((emulate) (root-emulate (cadr args)))
             (else (display "A typo? Almost, try again...!\n")
                   (exit 1))))))

   )
