;; A permutation is an ordered arrangement of objects. For example, 3124 is one 
;; possible permutation of the digits 1, 2, 3 and 4. If all of the permutations 
;; are listed numerically or alphabetically, we call it lexicographic order. 
;; The lexicographic permutations of 0, 1 and 2 are:

;; 012   021   102   120   201   210

;; What is the millionth lexicographic permutation of the digits 
;; 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

;; 0: 0 1 2 3 4 5 6 7 8 9
;; 1: 0 1 2 3 4 5 6 7 9 8 +   9
;; 2: 0 1 2 3 4 5 6 8 7 9 +  81
;; 3: 0 1 2 3 4 5 6 8 9 7 +  18
;; 4: 0 1 2 3 4 5 6 9 7 8 +  81
;; 5: 0 1 2 3 4 5 6 9 8 7 +   9
;; 6: 0 1 2 3 4 5 7 6 8 9 + 702
;; 7: 0 1 2 3 4 5 7 6 9 8 +   9
;; 8: 0 1 2 3 4 5 7 8 6 9 + 171
;; 9: 0 1 2 3 4 5 7 8 9 6 +  27
;;10: 0 1 2 3 4 5 7 9 6 8 +  72
;;11: 0 1 2 3 4 5 7 9 8 6 +  18
;;12: 0 1 2 3 4 5 8 6 7 9 + 693
;;13: 0 1 2 3 4 5 8 6 9 7 +  18
;;14: 0 1 2 3 4 5 8 7 6 9 +  72
;;15: 0 1 2 3 4 5 8 7 9 6 +  27
;;16: 0 1 2 3 4 5 8 9 6 7 + 171
;;17: 0 1 2 3 4 5 8 9 7 6 +   9
;;18: 0 1 2 3 4 5 9 6 7 8 + 702
;;19: 0 1 2 3 4 5 9 6 8 7 +   9
;;20: 0 1 2 3 4 5 9 7 6 8 +  81
;;21: 0 1 2 3 4 5 9 7 8 6 +  18
;;22: 0 1 2 3 4 5 9 8 6 7 +  81
;;23: 0 1 2 3 4 5 9 8 7 6 +   9
;;24: 0 1 2 3 4 6 5 7 8 9 +5913
(def a [9 81 18 81 9 702 9 171 27 72 18 693 18 72 27 171 9 702 9 81 18 81 9 5913])
