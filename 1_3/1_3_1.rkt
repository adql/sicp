#lang sicp

;;; ---------------------------------------------------------
;;; 1.3 Formulating Abstractions with Higher-Order Procedures
;;; ---------------------------------------------------------

;;; 1.3.1 Procedures as Arguments
;;; -----------------------------

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (identity x) x)

(define (inc n) (+ n 1))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))
(* 8 (pi-sum 1 10000))
(* 8 (pi-sum 1 100000))
(* 8 (pi-sum 1 1000000))
(* 8 (pi-sum 1 10000000))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (cube x) (* x x x))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
(integral cube 0 1 0.0001)

;; Exercise 1.29

(define (simpsons-rule f a b n)
  ;; first, make sure n is even
  (if (= (remainder n 2) 1) (simpsons-rule f a b (+ n 1)))
  (define h (/ (- b a) n))
  (define (sum-iter k)
    (define factor
      (cond ((or (= k 0) (= k n)) 1)
            ((= (remainder k 2) 1) 4)
            (else 2)))
    (if (> k n)
        0
        (+ (* factor (f (+ a (* k h))))
           (sum-iter (+ k 1)))))
  (* (/ h 3) (sum-iter 0)))

(simpsons-rule cube 0 1.0 100)
(simpsons-rule cube 0 1.0 1000)

;; Exercise 1.30

(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;testing
(sum2 identity 1 inc 10)
(sum2 cube 1 inc 2)

;; Exercise 1.31

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

;testing
(product identity 1 inc 5)
(product cube 1 inc 3)

(define (factorial n)
  (product identity 1 inc n))

;testing
(factorial 6)
(factorial 9)

(define (pi-wallis n)
  (define (term x)
    (/ (+ x (if (even? x) 2.0 3.0))
       (+ x (if (even? x) 3.0 2.0))))
  (* 4 (product term 0 inc n)))

;testing
(pi-wallis 100)
(pi-wallis 1000)
(pi-wallis 100000000)

;;b. recursive version of product
(define (product2 term a next b)
  (if (> a b)
      1
      (* (term a) (product2 term (next a) next b))))

;; Exercise 1.32

(define (accumulate combiner null-value
                    term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (summation a b) (+ a b))
(define (multiplication a b) (* a b))

(accumulate summation 0 identity 1 inc 10)
(accumulate multiplication 1 identity 1 inc 6)

;and the recursive version...
(define (accumulate2 combiner null-value
                     term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate2 combiner null-value
                                      term (next a) next b))))

(accumulate2 summation 0 identity 1 inc 10)
(accumulate2 multiplication 1 identity 1 inc 6)

;; Exercise 1.33

(define (filtered-accumulate combiner null-value filter
                             term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner (if (filter a) (term a) null-value)
                        result))))
  (iter a null-value))

;;prime? predicate from previous section

(define (square x) (* x x))

(define (expmod base exp m)
  (define (non-trivial-sqrt? b sqr_remainder)
    (and (not (= b 1))
         (< b (- m 1))
         (= sqr_remainder 1)))
  (cond ((= exp 0) 1)
        ((even? exp)
         (let* ((b (expmod base (/ exp 2) m))
                (sqr_remainder (remainder (square b) m)))
           (if (non-trivial-sqrt? b sqr_remainder) 0 sqr_remainder)))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (if (< n 4294967088)
                           (- n 1) 4294967087)))))

(define (fast-prime? n times)
  (cond ((= n 1) false)
        ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? x) (fast-prime? x 5))

;;finally question a
(define (sum-prime-squares a b)
  (filtered-accumulate summation 0 prime? square a inc b))

(sum-prime-squares 1 12)                ;208

;;b
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-of-all-the-positive-integers-less-than-n-that-are-relatively-prime-to-n n)
  (define (relatively-prime? x)
    (= (gcd x n) 1))
  (filtered-accumulate multiplication 1 relatively-prime? identity 1 inc (- n 1)))

(product-of-all-the-positive-integers-less-than-n-that-are-relatively-prime-to-n 10) ;189
(product-of-all-the-positive-integers-less-than-n-that-are-relatively-prime-to-n 20) ;8729721
