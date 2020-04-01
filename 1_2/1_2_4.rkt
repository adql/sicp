#lang sicp

;;; -----------------------------------------------
;;; 1.2  Procedures and the Processes They Generate
;;; -----------------------------------------------

;;; 1.2.4 Exponentiation
;;; --------------------

;;; Exercise 1.16

(define (expt b n)
  (define (even? n)
    (= (remainder n 2) 0))
  (define (fast-expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter (* b b) (/ n 2) a))
          (else (fast-expt-iter b (- n 1) (* a b)))))
  (fast-expt-iter b n 1))

;;; Exercise 1.17

(define (double a) (* a 2))
(define (halve a) (/ a 2))
(define (even? a) (= (remainder a 2) 0))

(define (mult a b)
  (cond ((= b 0) 0)
        ((even? b) (mult (double a) (halve b)))
        (else (+ a (mult a (- b 1))))))

;;; Exercise 1.18

(define (mult2 a b)
  (define (mult-iter a b c)
    (cond ((= b 0) c)
          ((even? b) (mult-iter (double a) (halve b) c))
          (else (mult-iter a (- b 1) (+ a c)))))
  (mult-iter a b 0))

;;; Exercise 1.19
(define (fib3 n)
  (fib-iter3 1 0 0 1 n))

;; First attempt, stupid algebra
;; (define (fib-iter a b p q count)
;;   (cond ((= count 0) b)
;;         ((even? count)
;;          (fib-iter a
;;                    b
;;                    (+ (* a p p) (* (+ (* 2 a) b) q q) (* 2 (+ a b) p q))
;;                    (+ (* b p p) (* (+ a b) q q) (* 2 a p q))
;;                    (/ count 2)))
;;         (else (fib-iter (+ (* b q) (* a q) (* a p))
;;                         (+ (* b p) (* a q))
;;                         p
;;                         q
;;                         (- count 1)))))

(define (fib-iter3 a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter3 a
                    b
                    (+ (* p p) (* q q))
                    (+ (* 2 q p) (* q q))
                    (/ count 2)))
        (else (fib-iter3 (+ (* b q) (* a q) (* a p))
                         (+ (* b p) (* a q))
                         p
                         q
                         (- count 1)))))

;;; Exercise 1.20

;; normal order
;; (gcd 206 40)
;; (if (= 40 0)
;;     206
;;     (gcd 40 (remainder 206 40)))
;; (if (= 40 0)
;;     206
;;     (if (= (remainder 206 40) 0)
;;         40
;;         (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))))
;; (if (= 40 0)
;;     206
;;     (if (= (remainder 206 40) 0)
;;         40
;;         (if (= (remainder 40 (remainder 206 40)) 0)
;;             (remainder 206 40)
;;             (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))
;; ...
;; Infinite. "remainder" is never evaluated
;; Wrong! In an if statement the condition is first evaluated.

;; applicative order
;; (gcd 206 40)
;; (if (= 40 0)
;;     206
;;     (gcd 40 (remainder 206 40)))
;; (gcd 40 (remainder 206 40))
;; (gcd 40 6)                              ;1
;; (if (= 6 0)
;;     40
;;     (gcd 6 (remainder 40 6)))
;; (gcd 6 (remainder 40 6))
;; (gcd 6 4)                               ;2
;; (if (= 4 0)
;;     6
;;     (gcd 4 (remainder 6 4)))
;; (gcd 4 (remainder 6 4))
;; (gcd 4 2)                               ;3
;; (if (= 2 0)
;;     4
;;     (gcd 2 (remainder 4 2)))
;; (gcd 2 (remainder 4 2))
;; (gcd 2 2)                               ;4
;;
;; !!! Stupid !!!
;;
;; (if (= 2 0)
;;     2
;;     (gcd 2 (remainder 2 2)))
;; (gcd 2 (remainder 2 2))
;; (gcd 2 0)                               ;5
;; (if (= 0 0)
;;     2
;;     (gcd 0 (remainder 2 0)))
;; 2
;; remainder evaluated 5 times
