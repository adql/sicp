#lang sicp

;;; --------------------------
;;; Assignment and Local State
;;; --------------------------


;;; 3.1.2 The Benefits of Introducing Assignment
;;; --------------------------------------------

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;;; Exercise 3.5
;;; ------------

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (square x) (* x x))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (let ((square-area (* (- x2 x1) (- y2 y1))))
    (* (monte-carlo trials experiment) square-area)))

;for estimating pi
(define (estimate-pi-with-integral)
  (define (P x y)
    (<= (+ (square x) (square y)) 1.0)) ;unit circle
  (estimate-integral P -1.0 1.0 -1.0 1.0 1e8))

;test
(estimate-pi-with-integral)

;;; Exercise 3.6

(define random-init 1.0)

(define (rand-update x) (+ x 1))      ;just for demonstration

(define rand
  (let ((x random-init))
    (lambda (arg)
      (cond ((eq? arg 'generate)
             (set! x (rand-update x))
             x)
            ((eq? arg 'reset)
             (lambda (new-value)
               (set! x new-value)))
            (else
             (error "Invalid call -- RAND" arg))))))

;test
(rand 'generate)                        ;2.0
(rand 'generate)                        ;3.0
((rand 'reset) 5.0)
(rand 'generate)                        ;6.0
