# `versus`

## BiwaScheme Reference

### Basic

```scheme
%macroexpand %macroexpand-1 * + - ... / < <= = > >= abs acos and angle
append apply asin assoc assp assq assv atan boolean= boolean? caar
cadr call-with-values car case cdddar cddddr cdr ceiling char->integer
char<=? char<? char=? char>=? char>? char? complex? cond cons cons*
cos define-enumeration define-record-type denominator div div0
div0-and-mod0 div0-and-mod0 do dotimes enum-set->list
enum-set-complement enum-set-difference enum-set-intersection
enum-set-member? enum-set-projection enum-set-subset? enum-set-union
enum-set=? eq? equal-hash equal? eqv? eval even? exact-integer-sqrt
exists exp expt filter find finite? floor fold-left fold-right for-all
for-each format gensym get-output-string hashtable-clear!
hashtable-contains? hashtable-copy hashtable-delete! hashtable-entries
hashtable-equivalence-function hashtable-hash-function hashtable-keys
hashtable-mutable? hashtable-ref hashtable-set! hashtable-size
hashtable-update! hashtable? if imag-part infinite? integer->char
integer? iota length let let let* let-values let1 let\*-values letrec
letrec* list list->string list->vector list-copy list-ref list-sort
list-tail list? log macroexpand macroexpand-1 magnitude
make-eq-hashtable make-eqv-hashtable make-hashtable make-polar
make-rectangular make-string map max member memp memq memv min mod
mod0 nan? negative? not null? number->string number? numerator odd?
open-input-string open-output-string or pair? partition positive?
raise random-integer random-real rational? rationalize real-part
receive remove remp remq remv reverse round set-car! set-cdr! sin sqrt
string string->list string->number string->symbol string-append
string-ci-hash string-copy string-for-each string-hash string-length
string-ref string<=? string<? string=? string>=? string>? string?
substring symbol->string symbol-hash symbol=? symbol? tan truncate
unless values vector vector->list vector-append vector-copy
vector-fill! vector-for-each vector-length vector-map vector-ref
vector-set! vector-sort vector-sort! vector? when zero?
```

### `define-macro`

```scheme
(define-macro (test expr)
  `(if ,expr
       #t
       (print (format "test failed: ~a" (quote ,expr)))))
```

### JavaScript Interface

```scheme
(alert msg)
(alist->js-obj)
(clear-timer! timer-id) ;; => clearInterval
(confirm msg)
(console-debug obj1 ...) ;; => console.debug
(console-error obj1 ...)
(console-info obj1 ...)
(console-log obj1 ...)
(console-warn obj1 ...)
(element-add-class-name! elem class)
(element-append-child! elem child)
(element-content elem) ;; => string (html content or input value)
(element-dimensions elem) ;; => (values width height)
(element-empty! elem) ;; => (element-clear! elem)
(element-focus! elem)
(element-has-class-name? elem class)
(element-height elem)
(element-hide! elem)
(element-insert! elem x)
(element-new '("div#main" "foo")) ;; => <div id='main'>foo</div>
(element-new '("div.red" "foo"))  ;; => <div class='red'>foo</div>
(element-new '(div "foo"))        ;; => <div>foo</div>
(element-new '(div (span "foo")))  ;; => <div><span>foo</span></div>
(element-new '(div align "right" "foo"))  ;; => <div align='right'>foo</div>
(element-new spec)
(element-read-attribute elem attr)
(element-remove! elem)
(element-remove-class-name! elem class)
(element-replace! elem x)
(element-select elem)
(element-show! elem)
(element-toggle! elem)
(element-toggle-class-name! elem class)
(element-update! elem html)
(element-visible? elem)
(element-width elem)
(element-write-attribute! elem attr value)
(getelem selector) ;; => ($ selector)
(js-array->list)
(js-call jsfunc args...) ;; => a()
(js-closure proc)
(js-eval str) ;; evaluate str as JavaScript code
(js-function? x)
(js-invocation)
(js-invoke jsobj methodname args...) ;; => a.b()
(js-new ctor args...) ;; => new a
(js-new ctorname args...) ;; => new a
(js-null? x)
(js-obj key1 value1 key2 value2...)
(js-obj->alist)
(js-ref jsobj str) ;; => a.b
(js-set! jsobj str value) ;; => a.b ;; => c
(js-undefined? x)
(list->js-array)
(set-content! selector text)
(set-timer! proc sec) ;; => setInterval
(sleep sec)
(timer proc sec) ;; => setTimeout
```
