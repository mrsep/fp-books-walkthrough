#lang br/quicklang

; the reader
(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums '(handle ~a) src-lines)) ; convert source code to S-exp
  (define module-datum `(module stacker-mod "stacker.rkt"
                          ,@src-datums)) ; construct code module (list of S-exp)
  (datum->syntax #f module-datum))       ; transform code module to syntax object

(provide read-syntax)

; the expander
(define-macro (stacker-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     HANDLE-EXPR ...
     (display (first stack))))

(provide (rename-out [stacker-module-begin #%module-begin]))

; internal data management of the language stacker
(define stack empty)

(define (pop-stack!)
  (define arg (first stack))
  (set! stack (rest stack))
  arg)

(define (push-stack! arg)
  (set! stack (cons arg stack)))

; public API of the stacker language
(define (handle [arg #f])
  (cond
    [(number? arg) (push-stack! arg)]
    [(or (equal? + arg) (equal? * arg))
     (define op-result (arg (pop-stack!) (pop-stack!)))
     (push-stack! op-result)]))

(provide handle)

(provide * +)
