#lang racket

(module+ test
  (require rackunit))

(define (our-div num denom)
  (/ num denom))

(module+ test
  (check-equal? (our-div 42 6) 7)
  (check-equal? (our-div 42 2) 21)
  (check-exn exn:fail? (lambda () (our-div 42 0))))

(define (our-mult factor1 factor2)
  (* factor1 factor2))

(module+ test
  (check-equal? (our-mult 6 7) 42)
  (check-exn exn:fail? (lambda () (our-mult "a" "b"))))
