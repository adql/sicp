#lang sicp

;;; ---------------------------------------------------------
;;; 1.3 Formulating Abstractions with Higher-Order Procedures
;;; ---------------------------------------------------------

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

;;; 1.3.4 Procedures as Returned Values
;;; -----------------------------------

;;; Newton's Method

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; Exercise 1.40

; x^3 + ax^2 + bx +c = 0
; x |-> cube-root (- (ax^2 + bx + c))

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (cube-root x)
  (newtons-method (lambda (y) (- (cube y) x)) 1.0))

;; wrong attempt
;; (define (cubic a b c)
;;   (lambda (x) (cube-root (* -1 (+ (* a (square x))
;;                                   (* b x)
;;                                   c)))))

;; Read solution online. This was confusing because I was expected
;; something of a transformation similar to y |-> y^2 -x while
;; actually there is no y here, therefore it's just the cubic equation
;; alone equaling to 0, and the method tries to find x.

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic 1 1 1) 1)
(newtons-method (cubic 2 5 -3) 1)

;; Exercise 1.41

(define (double proc)
  (lambda (arg) (proc (proc arg))))

((double inc) 3)                        ;5

(((double (double double)) inc) 5)      ;21

;; Exercise 1.42

(define (compose f g)
  (lambda (arg) (f (g arg))))

((compose square inc) 6)                ;49

;; Exercise 1.43

;; (define (identity x)
;;   x)

(define (repeated f n)
  (define (iter final i)
    (if (= i n)
        final
        (iter (compose f final) (+ i 1))))
  (if (= n 0) identity (iter f 1)))

((repeated square 2) 5)                 ;625
((repeated square 0) 5)                 ;5

;; Exercise 1.44

(define (smooth f)
  (let ((dx 0.00001))
    (lambda (x) (/ (+ (f x)
                      (f (- x dx))
                      (f (+ x dx)))
                   3))))

(define (some-function x)
  (+ (* 3 (square x)) (* -2 x) 7))
(define some-function-smoothed
  (smooth some-function))

(some-function 6)                       ;103
(some-function-smoothed 6)              ;103.00000000020002
(some-function 2)                       ;15
(some-function-smoothed 2)              ;15.000000000199998

(define (nth-smooth f n)
  ((repeated smooth n) f))

((nth-smooth some-function 5) 2)        ;15.000000000999998

;; Exercise 1.45

;;experiments:

(define (root-experiment n root n-damping)
  (fixed-point ((repeated average-damp n-damping)
                (lambda (y) (/ n ((repeated square (- root 2)) y))))
               2.0))

(root-experiment 5 2 1)
(root-experiment 5 3 1)
(root-experiment 5 4 2)
(root-experiment 5 5 3)
(root-experiment 5 6 4)
(root-experiment 5 7 5)
;; Seems like the average-damping has to be performed root-2 times for
;; n>=3, and once for n=2

(define (nth-root root n)
  (fixed-point ((repeated average-damp (if (= root 2) 1 (- root 2)))
                (lambda (y) (/ n ((repeated square (- root 2)) y))))
               2.0))

(nth-root 2 10)
(nth-root 5 10)

;; Exercise 1.46

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (define (iter guess)
      (if (good-enough? guess)
          guess
          (iter (improve guess))))
    (iter guess)))

(define (sqrt-iter-impr x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.00001))
  (define (improve guess)
    (/ (+ x guess) 2))
  ((iterative-improve good-enough? improve) 1.0))

(sqrt 4)                                ;2
(sqrt 2)                                ;1.414
(sqrt 10)                               ;3.162

(define (fixed-point-iter-impr f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? guess)
    (let ((next (f guess)))
      (< (abs (- guess next)) tolerance)))
  (define (try guess)
    (if (close-enough? guess)
        (f guess)
        (try (f guess))))
  (try first-guess))

(fixed-point-iter-impr cos 1.0)         ;0.7390822985224024
(fixed-point-iter-impr (lambda (y) (+ (sin y) (cos y))) 1.0)
;1.2587315962971173
