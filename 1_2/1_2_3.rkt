#lang sicp

;;; -----------------------------------------------
;;; 1.2  Procedures and the Processes They Generate
;;; -----------------------------------------------

;;; 1.2.3 Orders of Growth
;;; ----------------------

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle step)
  (display step) (display ": ") (display angle) (newline)o
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0) (+ step 1)))))

(display (sine 12.15 1))
