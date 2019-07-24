#lang br
(require "struct.rkt" "line.rkt")
(provide b-end b-goto b-gosub b-return)

(define (b-end) (raise (end-program-signal)))
(define (b-goto expr)
  (raise (change-line-signal expr)))

(define return-ccs empty)

(define (b-gosub num-expr)
  (let/cc here-cc
    (push! return-ccs here-cc)
    (b-goto num-expr)))

(define (b-return)
  (unless (not (empty? return-ccs))
    (raise-line-error "return without gosub"))
  (define top-cc (pop! return-ccs))
  (top-cc (void)))
