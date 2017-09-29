#lang br
(require jsonic/parser jsonic/tokenizer brag/support)

(parse-to-datum
 (apply-tokenizer-maker make-tokenizer
                        #<<DEREK
// comment
@$ 42 $@
@$ (* 6 7) $@
DEREK
                        ))

