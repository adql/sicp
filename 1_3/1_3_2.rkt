#lang sicp

;;; ---------------------------------------------------------
;;; 1.3 Formulating Abstractions with Higher-Order Procedures
;;; ---------------------------------------------------------

;;; 1.3.2 Constructing Procedures Using lambda
;;; ------------------------------------------

;; Exercise 1.34

(define (f g) (g 2))

(define (square n) (* n n))

(f square)

(f (lambda (z) (* z (+ z 1))))

;(f f)
;(f 2)
;(2 2)

(f f)

