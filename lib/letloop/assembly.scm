(library (letloop assembly)

  (export assembly->procedure
          ;; ~check-assembly-000
          ~benchmark-assembly-000 ~benchmark-assembly-001
          )

  (import (chezscheme))


  (define assembly->procedure
    (lambda (assembly)
      (define i0 (call-with-output-file "/tmp/letloop/assembly.s"
                   (lambda (port)
                     (put-string port assembly)) 'truncate))

      (define i1 (system (format #f "cc -shared -O3 /tmp/letloop/assembly.s -o /tmp/letloop/assembly.so")))

      (define i2 (load-shared-object "/tmp/letloop/assembly.so"))

      (foreign-procedure "fibas" (int) int)))

  (define pk
    (lambda args
      (write args)
      (newline)
      (car (reverse args))))

  (define fib
    (lambda (n)
      (cond
       ((= n 0) 0)
       ((= n 1) 1)
       (else (+ (fib (- n 1)) (fib (- n 2)))))))

  (define assembly "# Need to make it global so it can be accessed in another file with extern
.globl fibas

.type fibas, @function

fibas:
    mov %rdi, %rax
    cmp $1, %rdi
    jl fib_exit

    mov $0, %rax
    mov $0, %rbx
    mov $1, %rcx

fib_loop:
  add %rcx, %rbx
  mov %rbx, %rax
  mov %rcx, %rbx
  mov %rax, %rcx
  dec %rdi
  cmp $1, %rdi
  jg fib_loop

fib_exit:
    ret
")

  (define ~check-assembly-000
    (lambda ()

      (define fibas (assembly->procedure assembly))
      (for-each (lambda (n) (= (fib n)
                               (fibas n)))
                (iota 10))))

  (define ~benchmark-assembly-000
    (lambda ()
      (define fibas (assembly->procedure assembly))
      (fibas 42)))

  (define ~benchmark-assembly-001
    (lambda ()
      (define fibas (assembly->procedure assembly))
      (fib 42))))
