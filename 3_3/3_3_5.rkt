#lang sicp

;;; ------------------------------
;;; 3.3 Modeling with Mutable Data
;;; ------------------------------


;;; 3.3.5 Propagation of Constraints

;;; Implementing the constraint system


(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

;;; Representing connectors

     (define (make-connector)
       (let ((value false) (informant false) (constraints '()))
         (define (set-my-value newval setter)
           (cond ((not (has-value? me))
                  (set! value newval)
                  (set! informant setter)
                  (for-each-except setter
                                   inform-about-value
                                   constraints))
                 ((not (= value newval))
                  (error "Contradiction" (list value newval)))
                 (else 'ignored)))
         (define (forget-my-value retractor)
           (if (eq? retractor informant)
               (begin (set! informant false)
                      (for-each-except retractor
                                       inform-about-no-value
                                       constraints))
               'ignored))
         (define (connect new-constraint)
           (if (not (memq new-constraint constraints))
               (set! constraints
                     (cons new-constraint constraints)))
           (if (has-value? me)
               (inform-about-value new-constraint))
           'done)
         (define (me request)
           (cond ((eq? request 'has-value?)
                  (if informant true false))
                 ((eq? request 'value) value)
                 ((eq? request 'set-value!) set-my-value)
                 ((eq? request 'forget) forget-my-value)
                 ((eq? request 'connect) connect)
                 (else (error "Unknown operation -- CONNECTOR"
                              request))))
         me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;;; Exercise 3.33

(define (averager a b c)
  (let ((u (make-connector))
        (v (make-connector)))
    (adder a b u)
    (multiplier u v c)
    (constant 0.5 v)
    'ok))

;test
(define c1 (make-connector))
(define c2 (make-connector))
(define c3 (make-connector))
(averager c1 c2 c3)
(probe "c1" c1)
(probe "c2" c2)
(probe "c3" c3)
(set-value! c1 10 'user)                 ;Probe c1 = 10
(set-value! c2 20 'user)                 ;Probe c2 = 20, Probe c3 = 15.0
(set-value! c3 40 'user)                 ;Contradiction
(forget-value! c1 'user)                 ;Probe c1 = ?, Probe c3 = ?
(set-value! c3 40 'user)                 ;Probe c3 = 40, Probe c1 = 60.0

;;; Exercise 3.34

;Attempting this:
(define (squarer a b)
  (multiplier a a b))
;testing
(define c4 (make-connector))
(define c5 (make-connector))
(squarer c4 c5)
(probe "c4" c4)
(probe "c5" c5)
(set-value! c4 5 'user)                  ;Probe c4 = 5, Probe c5 = 25
;so far so good
(set-value! c5 100 'user)                ;Contradiction
;still good
(forget-value! c4 'user)                 ;Probe c4 = ?, Probe c5 = ?
(set-value! c5 100 'user)                ;Probe c5 = 100
;c4 is not updated to be 10 as expected.
;The problem is that the internal multiplier constraint expects a value for at
;least one of the multiplier. It is not "aware" that they are both the same.
;Hence it doesn't update c4 when c5 gets a value.

;;; Exercise 3.35

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (set-value! b (* (get-value a) (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

;test
(define c6 (make-connector))
(define c7 (make-connector))
(squarer c6 c7)
(probe "c6" c6)
(probe "c7" c7)
(set-value! c6 5 'user)                  ;Probe c6 = 5, Probe c7 = 25
(forget-value! c6 'user)                 ;Probe c6 = ?, Probe c7 = ?
(set-value! c7 100 'user)                ;Probe c7 = 100, Probe c6 = 10

;;; Exercise 3.37

(define (celsius-fahrenheit-converter-2 x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

;; (define C (make-connector))
;; (define F (celsius-fahrenheit-converter-2 C))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

;test
(define c8 (make-connector))
(define c9 (make-connector))
(define c10 (c* c8 c9))
(probe "c8" c8)
(probe "c9" c9)
(probe "c10" c10)
(set-value! c8 10 'user)                ;Probe c8 = 10
(set-value! c10 40 'user)               ;Probe c10 = 40, Probe c9 = 4

(define (cv x)
  (let ((y (make-connector)))
    (constant x y)
    y))

;test
(define c11 (cv 10))
(get-value c11)                         ;10

(define (c- x y)
  (let ((z (make-connector)))
    (c+ x
        (c* y (cv -1)))))

(define (c/ x y)
  (let ((z (make-connector))
        (y-inv (/ 1.0 (get-value y))))
    (c* x (cv y-inv))))

;test
(define C (make-connector))
(define F (celsius-fahrenheit-converter-2 C))
(probe "C" C)
(probe "F" F)
(set-value! C 36 'user)                 ;Probe C = 36, Probe F = 96.8
(forget-value! C 'user)                 ;Probe: C = ?, Probe: F = ?
(set-value! F 100 'user)                ;Probe: F = 100, Probe C = 37.777...
