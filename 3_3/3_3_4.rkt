#lang sicp

;;; ------------------------------
;;; 3.3 Modeling with Mutable Data
;;; ------------------------------


;;; 3.3.4 A Simulator for Digital Circuits

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;;; Primitive function boxes

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
(define (logical-and s1 s2)
  (cond ((not (or (= s1 0) (= s1 1)))
         (error "Invalid signal" s1))
        ((not (or (= s2 0) (= s2 1)))
         (error "Invalid signal" s2))
        ((and (= s1 1) (= s2 1)) 1)
        (else 0)))

;;; Exercise 3.28

(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)

;;; Exercise 3.29

(define (compound-or-gate o1 o2 output)
  (let ((a (make-wire)) (b (make-wire)) (c (make-wire)))
    (inverter o1 a)
    (inverter o2 b)
    (and-gate a b c)
    (inverter c output)
    'ok))

;; The delay time would be 2*inverter-delay + and-gate-delay

(define (ripple-carry-adder a b c-in s c-out)
  (define (iter a b c-in s)
    (if (null? (cdr a))
        (full-adder (car a) (car b) c-in (car s) c-out)
        (let ((c-out (make-wire)))
          (full-adder (car a) (car b) c-in (car s) c-out)
          (iter (cdr a) (cdr b) c-out (cdr s)))))
  (iter a b c-in s c-out))

;; Assuming that and-gate-delay+inverter-delay > or-gate-delay:
;; half-adder-delay = 2*and-gate-delay + inverter-delay
;; When the time-consuming process is due to the c-in, then:
;; full-adder-delay = 2*half-adder-delay + or-gate-delay
;;                  = 4*and-gate-delay + 2*inverter-delay + or-gate-delay
;; ripple-carry-adder-delay = n * (4 * and-gate-delay +
;;                                 2 * inverter-delay +
;;                                     or-gate-delay)
