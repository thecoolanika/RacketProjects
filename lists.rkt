#lang racket

;; Which functions are "public" (so that we can run/test them)
(provide prefix? sublist? enumerate)

;; prefix?
;;   Inputs: two lists, L and M
;;   Output: Whether L is a prefix of M
;;             i.e., whether all the elements of L appear consecutively
;;                   at the beginning of M, in the same order.
;;   Hint 1: use  equal?  to check if an element of L is the same as
;;           an element of M, since = only works for numbers
;;           and there's no guarantee L and M are lists of numbers.
;;   Hint 2: the code must work when one or both inputs are the empty list.
;;   Hint 3: given the specification above, the empty list is a
;;           prefix of all lists (including the empty list).
(define (prefix? L M)
   (if (empty? L) #t
       (if (empty? M) #f
           (if (equal? (first L) (first M)) (prefix? (rest L) (rest M)) #f))))
  
;; sublist?
;;   Inputs: two lists, L and M
;;   Output: Whether L is a sublist of M
;;            i.e., whether the elements of L appear consecutively and
;;                  in the same order, somewhere in M
;;   Hint: You can use prefix? as a helper function.
(define (sublist? L M)
   (if (empty? M) #f (if (prefix? L M) #t (sublist? L (rest M)))))

;; enumHelp
;; Helper Method for enumerate
;;   Inputs: a number for the list and a list (x0 x1 ... xn) 

;;   Outputs: a list of pairs ((0 x0) (1 x1) (2 x2) ... (n xn))
;;            where the input elements become pairs, sequentially
;;            numbered starting from 0.
(define (enumHelp n L)
  (if (equal? (length L) 1) (list(list n (first L))) (cons (list n (first L)) (enumHelp (+ n 1) (rest L))))
  )

;; enumerate
;;   Inputs: a list (x0 x1 ... xn)

;;   Outputs: a list of pairs ((0 x0) (1 x1) (2 x2) ... (n xn))
;;            where the input elements become pairs, sequentially
;;            numbered starting from 0.
;;   Hint: define a separate (non-public) helper function with more arguments,
;;         and have enumerate call it.
(define (enumerate L)
  (if (empty? L) '() (enumHelp 0 L))
  )