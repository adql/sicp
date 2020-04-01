#lang sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
 
;;; ------------------------------------
;;; 2.1 Introduction to Data Abstraction
;;; ------------------------------------


;;; 2.1.1 Example: Arithmetic Operations for Rational Numbers
;;; ---------------------------------------------------------

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;;; Representing rational numbers

;(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

;;; Exercise 2.1

;; (define (make-rat-better n d)
;;   (let ((g (gcd d n)))
;;     (cons (/ n g) (/ d g))))

;; (make-rat-better -2 -5)                 ;(2 . 5)
;; (make-rat-better 2 -5)                  ;(-2 . 5)
;; (make-rat-better -2 5)                  ;(-2 . 5)
;; (make-rat-better 6 -9)                  ;(-2 . 3)
;; (make-rat-better -6 9)                  ;(-2 . 3)
;; (make-rat-better -6 -9)                 ;(2 . 3)
;; (make-rat-better 1 -2)                  ;(1 . -2)
;Finally failed...

(define (make-rat-better n d)
  ;; g is defined as signed, then only n receives the sign
  (let ((g (* (abs (gcd n d))
              (/ (* n d) (abs (* n d))))))
    (cons (/ (abs n) g) (abs (/ d g)))))

(make-rat-better -2 -5)                 ;(2 . 5)
(make-rat-better 2 -5)                  ;(-2 . 5)
(make-rat-better -2 5)                  ;(-2 . 5)
(make-rat-better 6 -9)                  ;(-2 . 3)
(make-rat-better -6 9)                  ;(-2 . 3)
(make-rat-better -6 -9)                 ;(2 . 3)
(make-rat-better 1 -2)                  ;(-1 . 2)
