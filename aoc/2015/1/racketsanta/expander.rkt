#lang br/quicklang

(define-macro (santa-module-begin PARSE-TREE)
  #'(#%module-begin PARSE-TREE))

(provide (rename-out [santa-module-begin #%module-begin]))

(define-macro (santa-program LEFT-OR-RIGHT ...)
  #'(for/fold ([sum 0]
               [column 0])
              ([current-paren LEFT-OR-RIGHT ...]
               ;#:break (negative? sum)
               )
      (list (+ sum (first current-paren)) (second current-paren))))

(define-macro (left-paren STR)
  #'(1 (syntax-column STR)))

(define-macro (right-paren STR)
  #'(-1 (syntax-column STR)))

(provide santa-program)
(provide left-paren)
(provide right-paren)
