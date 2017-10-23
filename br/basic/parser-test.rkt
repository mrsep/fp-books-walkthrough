#lang br
(require basic/parser basic/tokenizer brag/support rackunit)

(define hello-world #<<HERE
10 print "hello" : print "world"
20 goto 9 + 10 + 11
30 end
HERE
)

(define program #<<HERE
30 rem print 'ignored'
35
50 print "never gets here"
40 end
60 print 'three' : print 1.0 + 3
70 goto 11. + 18.5 + .5 rem ignored
10 print "o" ; "n" ; "e"
20 print : goto 60.0 : end
HERE
)

(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer hello-world))
 '(b-program
   (b-line
    (b-line-num 10)
    (b-statement (b-print "print" (b-printable "hello")))
    ":"
    (b-statement (b-print "print" (b-printable "world"))))
   "\n"
   (b-line
    (b-line-num 20)
    (b-statement
     (b-goto
      "goto"
      (b-expr (b-sum (b-number 9) "+" (b-number 10) "+" (b-number 11))))))
   "\n"
   (b-line (b-line-num 30) (b-statement (b-end "end")))) )


(check-equal?
 (parse-to-datum (apply-tokenizer make-tokenizer program))
'(b-program
  (b-line (b-line-num 30) (b-rem "rem print 'ignored'"))
  "\n"
  (b-line (b-line-num 35))
  "\n"
  (b-line
   (b-line-num 50)
   (b-statement (b-print "print" (b-printable "never gets here"))))
  "\n"
  (b-line (b-line-num 40) (b-statement (b-end "end")))
  "\n"
  (b-line
   (b-line-num 60)
   (b-statement (b-print "print" (b-printable "three")))
   ":"
   (b-statement
    (b-print
     "print"
     (b-printable (b-expr (b-sum (b-number 1.0) "+" (b-number 3)))))))
  "\n"
  (b-line
   (b-line-num 70)
   (b-statement
    (b-goto
     "goto"
     (b-expr (b-sum (b-number 11.0) "+" (b-number 18.5) "+" (b-number 0.5)))))
   (b-rem "rem ignored"))
  "\n"
  (b-line
   (b-line-num 10)
   (b-statement
    (b-print
     "print"
     (b-printable "o")
     ";"
     (b-printable "n")
     ";"
     (b-printable "e"))))
  "\n"
  (b-line
   (b-line-num 20)
   (b-statement (b-print "print"))
   ":"
   (b-statement (b-goto "goto" (b-expr (b-sum (b-number 60.0)))))
   ":"
   (b-statement (b-end "end")))))
