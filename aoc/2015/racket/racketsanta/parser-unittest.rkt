#lang br
(require racketsanta/parser
         racketsanta/tokenizer
         brag/support
         rackunit)

(parse-to-datum
 (apply-tokenizer-maker make-tokenizer "((()))"))

(apply-tokenizer-maker make-tokenizer "((()))")
