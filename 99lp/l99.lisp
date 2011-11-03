;; P01 (*) Find the last box of a list.
(defun my-last (l)
  (cond
    ((null l) nil)
    ((null (cdr l)) (car l))
    (t (my-last (cdr l)))))

