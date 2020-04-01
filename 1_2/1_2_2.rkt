#lang sicp

;;; -----------------------------------------------
;;; 1.2  Procedures and the Processes They Generate
;;; -----------------------------------------------

;;; 1.2.2 Tree Recursion
;;; --------------------

;; Terrible recursive algorithm (exponential)
(define (fib1 n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib1 (- n 1))
                 (fib1 (- n 2))))))

;; Much better iterative algorithm (linear)
(define (fib2 n)
  (fib-iter2 1 0 n))

(define (fib-iter2 a b count)
  (if (= count 0)
      b
      (fib-iter2 (+ a b) a (- count 1))))

;;; Example: Counting change

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;;; Exercise 1.11

;;Recursive:
(define (f1 n)
  (if (< n 3)
      n
      (+ (f1 (- n 1)) (* 2 (f1 (- n 2))) (* 3 (f1 (- n 3))))))

;; Failed attempt to understand how to solve iteratively
;; (f 4)
;; (+ (f 3) (* 2 (f 2)) (* 3 (f 1)))
;; (+ ((+ (f 2) (* 2 (f 1)) (* 3 (f 0)))) (* 2 (f 2)) (* 3 (f 1)))

;; (f 5)
;; (+ (f 4) (* 2 (f 3)) (* 3 (f 2)))
;; (+ (+ (f 3) (* 2 (f 2)) (* 3 (f 1)))
;;   (* 2 ((+ (f 2) (* 2 (f 1)) (* 3 (f 0))))) (* 3 (f 2)))
;; (+ (+ ((+ (f 2) (* 2 (f 1)) (* 3 (f 0)))) (* 2 (f 2)) (* 3 (f 1)))
;;   (* 2 ((+ (f 2) (* 2 (f 1)) (* 3 (f 0))))) (* 3 (f 2)))

;; One solution from Schemewiki
(define (f n) (fi n 0 1 2)) 

(define (fi i a b c) 
   (cond ((< i 0) i) 
         ((= i 0) a) 
         (else (fi (- i 1) b c (+ c (* 2 b) (* 3 a)))))) 

;; Exercise 1.12

(define (pascal row n)
  ;; Where row=0 is the top of the triangle,
  ;; n = 0 is the leftmost element of a row.
  ;; So that always n <= row
  (if (or (= n 0) (= n row))
      1
      (+ (pascal (- row 1) (- n 1)) (pascal (- row 1) n))))
