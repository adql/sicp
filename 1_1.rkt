#lang sicp

(define (square x)
  (* x x))

;; (define (abs x)
;;   (cond ((> x 0) x)
;;         ((= x 0) 0)
;;         ((< x 0) (- x))))

;; ;;another way
;; (define (abs x)
;;   (cond ((< x 0) (- x))
;;         (else x)))

;;yet another way
(define (abs x)
  (if (< x 0)
      (- x)
      x))

;; Exercise 1.2
(/ (+ 5 1 (- 2 (- 3 (+ 6 (/ 1 3))))) (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3
(define (procedure a b c)
  (cond ((and (< a b) (< a c)) (+ (* b b) (* c c)))
        ((and (< b a) (< b c)) (+ (* a a) (* c c)))
        (else (+ (* a a) (* b b)))))

;; Exercise 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))


;;; 1.1.7  Example: Square Roots by Newton's Method
;;; -----------------------------------------------

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;;; Exercise 1.7

(define (better-good-enough? prev-guess guess)
  (< (/ (abs (- prev-guess guess)) guess) 0.0001))

(define (better-sqrt-iter prev-guess guess x)
  (if (better-good-enough? prev-guess guess)
      guess
      (better-sqrt-iter guess (improve guess x) x)))

(define (better-sqrt x)
  (better-sqrt-iter 0 1.0 x))

;;; Exercise 1.8

(define (improve-cbrt guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cbrt-iter prev-guess guess x)
  (if (better-good-enough? prev-guess guess)
      guess
      (cbrt-iter guess (improve-cbrt guess x) x)))

(define (cbrt x)
  (cbrt-iter 0 1.0 x))
