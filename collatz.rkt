#lang racket

;; Two functions will be treated as publicly accessible
;;   (so that we can test them)
(provide collatz collatz-count)

;; collatz: the "3N+1 function"
;;   Inputs: an integer, N
;;   Output: 3N+1, if N is odd
;;           N/2, if N is even
(define (collatz N)
  (if (even? N) (quotient N 2) (+ (* 3 N) 1)))

; collatz-count
;;   Inputs: an integer, N
;;   Output: the number of times we have to apply the collatz function
;;           to N before we get the number 1
(define (collatz-count N)
  (if (equal? N 1) 0 (+(collatz-count (collatz N))1)))
