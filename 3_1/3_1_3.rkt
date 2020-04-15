#lang sicp

;;; --------------------------
;;; Assignment and Local State
;;; --------------------------


;;; 3.1.3 The Costs of Introducing Assignment
;;; -----------------------------------------

;; modified solution to 3-3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (verify p)
    (if (eq? p password)
        expose
        #f))
  (define (expose m)
    (dispatch password m))
  (define (dispatch p m)
    (cond ((eq? m 'verify) (verify p))
          ((not (eq? p password))
           (lambda (m) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define (make-joint acc acc-pass new-pass)
  (let ((access (acc acc-pass 'verify)))
    (if access
        (lambda (p m) (if (eq? p new-pass)
                          (access m)
                          ("Incorrect password")))
        "Incorrect password for target account")))

;; The idea behind this implementation is that the joint account is ignorant of
;; the password of the original account, so if there's an exercise to change the
;; password this will be easier...

;test
(define peter-acc (make-account 1000 'funnypass))
(define paul-acc (make-joint peter-acc 'funnypass 'notfunny))
((paul-acc 'notfunny 'withdraw) 100)    ;900
((peter-acc 'funnypass 'deposit) 50)    ;950

;;; Exercise 3.8

