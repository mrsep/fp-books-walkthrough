#lang br/quicklang
(module reader br
  (require "reader.rkt")
  (provide read-syntax)
  (provide get-info)

  ; the function which is called by the ide (drracket)
  (define (get-info port src-mod src-line src-col src-pos)
    (define (handle-query key default)
      (case key
        ; dynamic-require imports functions at runtime if neede
        [(color-lexer)
         (dynamic-require 'jsonic/colorer  'color-jsonic)]
        [(drracket:indentation)
         (dynamic-require 'jsonic/indenter 'indent-jsonic)]
        [(drracket:toolbar-buttons)
         (dynamic-require 'jsonic/buttons  'button-list)]
        [else default]))
    handle-query))
