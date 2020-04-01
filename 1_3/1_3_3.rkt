#lang sicp

;;; ---------------------------------------------------------
;;; 1.3 Formulating Abstractions with Higher-Order Procedures
;;; ---------------------------------------------------------

;;; 1.3.3 Procedures as General Methods
;;; -----------------------------------

;;; Finding roots of equations by the half-interval method

(define (average a b) (/ (+ a b) 2))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

;;; Finding fixed points of functions

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

;; Exercise 1.35

; x^2 = x + 1
; x = (x + 1) / x = 1 + 1/x

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

;; Exercise 1.36

(define (fixed-point-verbose f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display "Guess: ") (display next) (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (display "First guess: ") (display first-guess) (newline)
  (try first-guess))

;;without damping
(fixed-point-verbose (lambda (x) (/ (log 1000) (log x))) 5.0)
;; First guess: 5.0
;; Guess: 4.29202967422018
;; Guess: 4.741863119908242
;; Guess: 4.438204569837609
;; Guess: 4.635299887107611
;; Guess: 4.50397811613643
;; Guess: 4.589989462723705
;; Guess: 4.53301150767844
;; Guess: 4.570475672855484
;; Guess: 4.545720389670642
;; Guess: 4.562024936588171
;; Guess: 4.551263234080531
;; Guess: 4.55835638768598
;; Guess: 4.553676852183342
;; Guess: 4.55676216434628
;; Guess: 4.554727130670954
;; Guess: 4.556069054770006
;; Guess: 4.555184018843625
;; Guess: 4.5557676565438205
;; Guess: 4.555382746639082
;; Guess: 4.55563658243586
;; Guess: 4.555469180245326
;; Guess: 4.555579577900997
;; Guess: 4.5555067722873686
;; Guess: 4.5555547860484085
;; Guess: 4.555523121789556
;; Guess: 4.555544003742869
;; Guess: 4.555530232469306
;; Guess: 4.555539314360711
;; 4.555539314360711
;; 28 guesses without average damping

;;with damping
(fixed-point-verbose (lambda (x) (average x (/ (log 1000) (log x)))) 5.0)

;; First guess: 5.0
;; Guess: 4.64601483711009
;; Guess: 4.571611286076025
;; Guess: 4.558294317536066
;; Guess: 4.556006022881116
;; Guess: 4.555615799731297
;; Guess: 4.555549342575593
;; Guess: 4.555538027101999
;; Guess: 4.5555361005218895
;; 4.5555361005218895
;; 8 guesses with average damping

;; Exercise 1.37

(define (cont-frac n d k)
  (define (recur i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))

;;procedure to try successive k values

(define (cont-frac-successive-k k)
  (let ((result (cont-frac (lambda (i) 1.0)
                           (lambda (i) 1.0)
                           k)))
    (display k) (newline)
    (if (< (abs (- result (/ 1 1.6180))) 0.00001)
        result
        (cont-frac-successive-k (+ k 1)))))

(cont-frac-successive-k 1)
;; in 11 steps reaches 0.6180555555555556

;; iterative version
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                11)

;; Exercise 1.38

(define (eulers-expansion)
  (define (d i) (if (= (remainder i 3) 2)
                    (* (/ (+ i 1) 3) 2)
                    1))
  (define (execute k prev)
    (let ((result (cont-frac-iter (lambda (i) 1.0)
                                  d
                                  k)))
      (if (< (abs (- result prev)) 0.00001)
          (+ result 2)
          (execute (+ k 1) result))))
  (execute 1 2))

(eulers-expansion)                      ;2.718283582089552

;; Exercise 1.39

(define (tan-cf x k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (if (= i 1) x (* x x))
                         (- (- (* i 2) 1) result)))))
  (iter k 0))

(define pi 3.141593)

(tan-cf (/ pi 4) 50)
(tan-cf (/ pi 2) 50)
(tan-cf 0.0001 50)
