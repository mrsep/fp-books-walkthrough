#lang br/quicklang
(module+ reader
  (provide read-syntax))

(define (parse-wires port)
  (for/list ([wire-str (in-lines port)])
    (format-datum '(wire ~a) wire-str)))

(define (read-syntax path port)
  (define wire-datums (parse-wires port))
  ; use quasi-syntax (prefix quasi-quote with #) to immeditely generate syntax object
  (define module-syntax #`(module wires-module wires/main
                            #,@wire-datums))
  ; But: remove bindings introduced by quasi-syntax (hygiene, lexical content)
  (strip-bindings module-syntax))

; this macro does nothing
;(define-macro (wires-module-begin WIRE-LIST)
;  #'(#%module-begin
;     WIRE-LIST))
;(provide (rename-out [wires-module-begin #%module-begin]))
; hence, we can also just reuse a module-begin macro from somewhere else:
(provide #%module-begin) ; from br/quicklang

(define-macro-cases wire
  [(wire ARG -> ID) #'(define/display (ID)
                        (val ARG))]
  [(wire OP ARG -> ID) #'(define/display (ID)
                           (OP (val ARG)))]
  [(wire ARG1 OP ARG2 -> ID) #'(define/display (ID)
                                 (OP (val ARG1)
                                     (val ARG2)))]
  [else #'(void)])
(provide wire)

(define-macro (define/display (ID) BODY)
  #'(begin
      (define (ID) BODY)
      ; this code has to run at the end after all defines have been evaluated
      ; hence the main submodule
      (module+ main
        (displayln (format "~a: ~a" 'ID (ID))))))

; this will be slow! because of recomputations
;(define (val num-or-wire)
;  (if (number? num-or-wire)
;      num-or-wire
;      (num-or-wire)))

; memorize the results
(define val
  (let ([val-cache (make-hash)]) ; let over lambda - a closure
    (lambda (num-or-wire)
      (if (number? num-or-wire)
          num-or-wire
          ; sets the value or retrieves it
          (hash-ref! val-cache num-or-wire num-or-wire)))))

(define (mod-16bit x) (modulo x 65536))
(define-macro (define-16bit ID PROC-ID)
  #'(define ID (compose1 mod-16bit PROC-ID)))

(define-16bit AND bitwise-and)
(define-16bit OR  bitwise-ior)
(define-16bit NOT bitwise-not)
(define-16bit LSHIFT arithmetic-shift)
(define (RSHIFT x y) (LSHIFT x (- y)))

(provide AND OR NOT LSHIFT RSHIFT)

