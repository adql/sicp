#lang sicp

;;; ------------------------------------------
;;; Hierarchical Data and the Closure Property
;;; ------------------------------------------


;;; Representing Sequences
;;; ----------------------

;;; List Operations

;;; Exercise 2.17

(define (last-pair l)
  (if (null? (cdr l)) l (last-pair (cdr l))))

(last-pair (list 23 72 149 34))         ;'(34)

;;; Exercise 2.18

(define (reverse l)
  (define (iter l result)
    (if (null? l)
        result
        (iter (cdr l) (cons (car l) result))))
  (iter l '()))

(reverse (list 1 4 9 16 25))            ;'(25 16 9 4 1)

;;; Exercise 2.19

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (first-denomination coin-values)
  (define (iter l result)
    (cond ((null? l) result)
          ((> (car l) result) (iter (cdr l) (car l)))
          (else (iter (cdr l) result))))
  (iter (cdr coin-values) (car coin-values)))

(define (except-first-denomination coin-values)
  (letrec ((fst-denom (first-denomination coin-values))
           (iter (lambda (checked left)
                   (if (= (car left) fst-denom)
                       (append checked (cdr left))
                       (iter (append checked (list (car left))) (cdr left))))))
    (iter '() coin-values)))

(define (no-more? coin-values)
  (null? coin-values))

(cc 100 us-coins)                       ;292
(cc 100 uk-coins)                       ;104561

(define us-coins-reordered (list 10 50 5 25 1))
(cc 100 us-coins-reordered)             ;292

;The order of the coins doesn't affect the answer, since by definition
;and implementation it always searchs for the first-denomination, not
;simple the first element of the list.

;;; Exercise 2.20

(define (same-parity x . nums)
  (define (refine parity nums result)
    (cond ((null? nums) result)
          ((eq? (even? (car nums)) parity)
           (refine parity (cdr nums) (append result (list (car nums)))))
          (else (refine parity (cdr nums) result))))
  (cons x (refine (even? x) nums '())))

(same-parity 1 2 3 4 5 6 7)             ;'(1 3 5 7)
(same-parity 2 3 4 5 6 7)               ;'(2 4 6)

;;; Mapping over lists

;;; Exercise 2.21

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list-map items)
  (map square items))

;;; Exercise 2.23

(define (for-each f l)
  (f (car l))
  (if (null? (cdr l))
      true
      (for-each f (cdr l))))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))
