#lang sicp

;;; ------------------------------------
;;; 2.1 Introduction to Data Abstraction
;;; ------------------------------------


;;; 2.1.2 Abstraction Barriers
;;; --------------------------

;;; Exercise 2.2

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment s)
  (let ((p1 (start-segment s))
        (p2 (end-segment s)))
    (make-point (/ (+ (x-point p1) (x-point p2)) 2.0)
                (/ (+ (y-point p1) (y-point p2)) 2.0))))

(print-point
 (midpoint-segment (make-segment (make-point 1.0 -3.0)
                                 (make-point 4.0 6.0))))

;;; Exercise 2.3

;; ;parameters are top-left and bottom-right points
;; (define (make-rectangle tl br)
;;   (cons tl br))

;; (define (tl-rectangle r)
;;   (car r))
;; (define (br-rectangle r)
;;   (cdr r))
;; (define (tr-rectangle r)
;;   (make-point (x-point (br-rectangle r))
;;               (y-point (tl-rectangle r))))
;; (define (bl-rectangle r)
;;   (make-point (x-point (tl-rectangle r))
;;               (y-point (br-rectangle r))))

;silly because the rectangle is not rotated, but anyway...
(define (segment-length s)
  (define (square x) (* x x))
  (let ((start (start-segment s))
        (end (end-segment s)))
    (sqrt (+ (square (- (x-point end) (x-point start)))
             (square (- (y-point end) (y-point start)))))))

(define (rectangle-size r)
  (cons (segment-length (make-segment (tl-rectangle r) (bl-rectangle r)))
        (segment-length (make-segment (tl-rectangle r) (tr-rectangle r)))))

(define (perimeter-rectangle r)
  (let ((size (rectangle-size r)))
    (* 2 (+ (car size) (cdr size)))))

(define (area-rectangle r)
  (let ((size (rectangle-size r)))
    (* (car size) (cdr size))))

;; ;testing
;; (define r (make-rectangle (make-point 1 1) (make-point 4 3)))
;; ;3×2 rectangle
;; (perimeter-rectangle r)                 ;10
;; (area-rectangle r)                      ;6

;;now reimplementing rectangle as two parallel segments
(define (make-rectangle tl br)
  (let ((tr (make-point (x-point br) (y-point tl)))
        (bl (make-point (x-point tl) (y-point br))))
    (cons (make-segment tl tr) (make-segment bl br))))

(define (tl-rectangle r)
  (start-segment (car r)))
(define (tr-rectangle r)
  (end-segment (car r)))
(define (bl-rectangle r)
  (start-segment (cdr r)))
(define (br-rectangle r)
  (end-segment (cdr r)))

;exactly the same test
(define r (make-rectangle (make-point 1 1) (make-point 4 3)))
;3×2 rectangle
(perimeter-rectangle r)                 ;10
(area-rectangle r)                      ;6

