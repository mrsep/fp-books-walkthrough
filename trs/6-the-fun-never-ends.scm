;; The Reasoned Schemer
;; Chapter 6: The fun never ends

(load "mk.scm")
(load "mkextraforms.scm")
(load "mkprelude.scm")

; the goals which succeed and fail
(define U fail)
(define S succeed)

; = = = = = = = = = = = = = = = = = = = = = = = = = =

; succeeds iff the goal succeeds
(define anyo
  (lambda (g)
    (conde
     (g S)
     (else (anyo g)))))

; does never succeed
(define nevero
  (anyo U))

; allways succeeds
(define alwayso (anyo S))

(run* (q)
      S
      (== #t q))
; > (#t)

; one answer with S, but alwayso would produce 
; an infinite list.
(run 5 (q)
     alwayso
     (== #t q))
; > (#t #t #t #t #t)

;; What is the difference between alwayso and succeed?
;; S succeeds only once, but alwayso 
;; can succeed any number of times.

; succeed at least once
(define salo
  (lambda (g)
    (conde
     (S S)
     (else g))))

; This question would produce arbitrary many solutions #t.
(run 1 (q)
     (salo alwayso)
     (== #t q))
; > (#t)

; This question would run forever in producing 
; a second value, since the determination of the
; second value never finishes. This is the case 
; since the second conde line in salo trys nevero.
; Reconsider the law of conde.
(run 1 (q)
     (salo nevero)
     (== #t q))
; > (#t)

; The following questions (with 1 instead of 0) 
; never finish, and thus dont have an answer.

; When the fail in the second lines occurs, run tries
; again the first line and pretends that the first line
; of the conde in salo fails and runs nevero, which 
; never finishes.
(run 0 (q)
     (salo nevero)
     U
     (== #t q))

; Run restarts every time hitting the second goal, 
; which fails every time.
(run 0 (q)
     alwayso
     U
     (== #t q))

; q gots associated with #f, alwayso succeeds, but the 
; last goal fails, since q is already associated with #f.
; Thus run restarts, etc.
(run 0 (q)
     (conde
      ((== #f q) alwayso)
      (else (anyo (== #t q))))
     (== #t q))


; condi coms into play

; condi tries the second line after the failure 
; with the first line.
(run 1 (q)
     (condi
      ((== #f q) alwayso)
      (else (== #t q)))
     (== #t q))
; > (#t)
; But more answers are not possible, since the 
; second condi line is out of values, and thus 
; the process would never finish.

; Here the second condi line produces as many 
; values as are requested.
(run 5 (q)
     (condi
      ((== #f q) alwayso)
      (else (anyo (== #t q))))
     (== #t q))
; > (#t #t #t #t #t)

;; = = = = =  The Law of condi  = = = = =
;; condi behaves like conde, except that
;; its values are interleaved.
;; = = = = = = = = = = = = = = = = = = =

; Again teacupo:
(define teacupo
  (lambda (x)
    (conde
     ((== 'tea x) S)
     ((== 'cup x) S)
     (else U))))

(run 5 (r)
     (condi
      ((teacupo r) S)
      ((== #f r) S)
      (else U)))
; > (tea #f cup)

; run fails to produce a value with the first condi
; line, thus condi tries the second line, which every
; time produces a value. conde would not produce a 
; value, since it always tries the first conde line, 
; which succeeds, but the outer goal fails.
(run 5 (q)
     (condi
      ((== #f q) alwayso)
      ((== #t q) alwayso)
      (else U))
     (== #t q))
; > (#t #t #t #t #t)

; The next question is the other way around.
; To produce a value, run asks the first goal
; conde, which first question in every round 
; succeeds, thus conde succeeds and the second
; goal produces the according value.
; On the other hand condi would produce one
; value from the first question, but if more 
; values are requested, the second is in question
; which would never finish to evaluate.
(run 5 (q)
     (condi
      (alwayso S)
      (else nevero))
     (== #t q))
; (#t #t #t #t #t)

;; The goals of all must succeed for 
;; the all to succeed.

; This question does not produce a value.
(run 1 (q)
     (all
      (conde
       ((== #f q) S)
       (else (== #t q)))
      alwayso)
     (== #t q))

; But this one does
(run 5 (q)
     (alli
      (conde
       ((== #f q) S)
       (else (== #t q)))
      alwayso)
     (== #t q))
; > (#t #t #t #t #t)

; In every round, the first conde line is questioned.
(run 5 (q)
     (all
      (conde
       (S S)
       (else nevero))
      alwayso)
     (== #t q))
; > (#t #t #t #t #t)

; The first value is #t, but to get more values, alli
; asks the second line of the conde, whcih never halts.
(run 5 (q)
     (alli
      (conde
       (S S)
       (else nevero))
      alwayso)
     (== #t q))

;;;
'()
