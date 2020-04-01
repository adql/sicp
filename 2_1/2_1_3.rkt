#lang sicp

;;; ------------------------------------
;;; 2.1 Introduction to Data Abstraction
;;; ------------------------------------


;;; 2.1.3 What is Meant by Data?
;;; ----------------------------

;;; Exercise 2.5

(define (my-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (extract-expt n e)
  (define (iter n result)
    (if (= (remainder n e) 0)
        (iter (/ n e) (inc result))
        result))
  (iter n 0))

(define (my-car z)
  (extract-expt z 2))

(define (my-cdr z)
  (extract-expt z 3))

;testing
(my-car (my-cons 6 13))                       ;6
(my-cdr (my-cons 6 13))                       ;13

;;; Exercise 2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define (+ a b)
  (lambda (f)
    (lambda (x)
      (((a b) f) x))))

;;;; FAILED. This is Mindfuck™
