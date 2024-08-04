(import (chezscheme))
(import (letloop root))


;; helpers

(define pk
  (lambda args
    (display ";; " (current-error-port))
    (write args (current-error-port))
    (newline (current-error-port))
    (car (reverse args))))
  
(define tmp (root-temporary-directory "/tmp/letloop-root/bookbook"))
(root-init-exec "debian" "bookworm" "amd64" tmp)
(root-exec-exec tmp #f #f "/bin/bash")
(root-spawn-exec tmp)

;; Qemu system support: install linux-image-amd64, add fsRoot as /
;; /etc/fstab, and include 9p in initrd
;;
;; ref: https://superuser.com/a/536352/115319
;;
;; (root-emulate tmp)
