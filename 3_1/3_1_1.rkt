#lang sicp

;;; --------------------------
;;; Assignment and Local State
;;; --------------------------


;;; 3.1.1 Local State Variables
;;; ---------------------------

;;; Exercise 3.1

(define (make-accumulator sum)
  (lambda (acc)
    (set! sum (+ sum acc))
    sum))

;test
(define A (make-accumulator 5))
(A 10)                                  ;15
(A 10)                                  ;25

;;; Exercise 3.2

(define (make-monitored f)
  (let ((count 0))
    (lambda (call)
      (cond ((eq? call 'how-many-calls?)
             count)
            ((eq? call 'reset-count)
             (set! count 0))
            (else (begin (set! count (inc count))
                         (f call)))))))

;test
(define s (make-monitored sqrt))
(s 100)                                 ;10
(s 'how-many-calls?)                    ;1
(s 'reset-count)
(s 'how-many-calls?)                    ;0

;;; Exercise 3.3

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (cond ((not (eq? p password))
           (lambda (m) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

;test
(define acc (make-account 100 'hwahwaaa))
((acc 'hwahwaaa 'withdraw) 40)          ;60
((acc 'wrongpass 'deposit) 30)          ;"Incorrect password"

;;; Exercise 3.4

(define (call-the-cops)
  "wheeeooowheeeoooowheeeoooo")

(define (make-extra-safe-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((break-attempt-count 0))
    (define (secure amount)
      (if (>= break-attempt-count 7)
          (call-the-cops)
          (begin (set! break-attempt-count (inc break-attempt-count))
                 "Incorrect password")))
    (define (dispatch p m)
      (cond ((not (eq? p password))
             secure)
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch)))

;test
(define acc (make-extra-safe-account 100 'secret))
((acc 'secret 'withdraw) 40)            ;60
((acc 'wrong 'withdraw) 40)             ;calls the cops after 7 attempts
