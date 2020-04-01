#lang sicp

;;; ------------------------------------
;;; 2.1 Introduction to Data Abstraction
;;; ------------------------------------


;;; 2.1.4 Extended Exercise: Interval Arithmetic
;;; --------------------------------------------

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; (define (div-interval x y)
;;   (mul-interval x
;;                 (make-interval (/ 1.0 (upper-bound y))
;;                                (/ 1.0 (lower-bound y)))))

;;; Exercise 2.7

(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))

;;; Exercise 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;;; Exercise 2.9

(define (interval-width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

;; ;first, the width of intervals' sum
;; (interval-width (add-interval x y))
;; (interval-width
;;  (make-interval (+ (lower-bound x) (lower-bound y))
;;                 (+ (upper-bound x) (upper-bound y))))
;; (/ (-
;;     (upper-bound (make-interval (+ (lower-bound x) (lower-bound y))
;;                                 (+ (upper-bound x) (upper-bound y))))
;;     (lower-bound (make-interval (+ (lower-bound x) (lower-bound y))
;;                                 (+ (upper-bound x) (upper-bound y)))))
;;    2)
;; ;let's call the bounds by names: xl, xh, yl, yh
;; (/ (-
;;     (upper-bound (make-interval (+ xl yl)
;;                                 (+ xh yh)))
;;     (lower-bound (make-interval (+ xl yl)
;;                                 (+ xh yh))))
;;    2)
;; ;then apply the upper and lower bounds:
;; (/ (- (+ xh yh) (+ xl yl)) 2)
;; ;now, the sum of the intervals' width is:
;; (+ (interval-width x) (interval-width y))
;; (+ (/ (- (upper-bound x) (lower-bound x)) 2)
;;    (/ (- (upper-bound y) (lower-bound y)) 2))
;; (+ (/ (- xh xl) 2) (/ (- yh yl) 2))
;;; bleehhhh... should have done it mathematically instread of failingly

;;; Exercise 2.10

(define (div-interval x y)
  (if (> (* (lower-bound y) (upper-bound y)) 0)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))
      (error "Can't divide by an interval that spans zero.")))

;;; Exercise 2.11

;;table of the nine possibilites
;0 (-.-)x(-.-) > upperXupper . lowerXlower
;1 (-.-)x(-.+) > lowerXupper . lowerXlower
;2 (-.-)x(+-+) > upperXlower . lowerXupper
;3 (-.+)x(-.-) > upperXlower . lowerXlower              
;4 (-.+)x(-.+) > lowerXupper|upperXlower . lowerXlower|upperXupper
;5 (-.+)x(+.+) > lowerXupper . upperXupper
;6 (+.+)x(-.-) > upperXlower . lowerXupper
;7 (+.+)x(-.+) > upperXlower . upperXupper
;8 (+.+)x(+.+) > lowerXlower . upperXupper

(define (mul-interval-9-way x y)
  (define (interval-case x)
    (if (< (lower-bound x) 0)
        (if (< (upper-bound x) 0) 0 1)
        2))
  (let* ((case (+ (* 3 (interval-case x)) (interval-case y)))
         (lower (cond ((= case 0) (* (upper-bound x) (upper-bound y)))
                      ((or (= case 1) (= case 5)) (* (lower-bound x) (upper-bound y)))
                      ((= case 4) (min (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))
                      ((= case 8) (* (lower-bound x) (lower-bound y)))
                      (else (* (upper-bound x) (lower-bound y)))))
         (upper (cond ((or (= case 0) (= case 1) (= case 3)) (* (lower-bound x) (lower-bound y)))
                      ((or (= case 2) (= case 6)) (* (lower-bound x) (upper-bound y)))
                      ((= case 4) (max (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
                      (else (* (upper-bound x) (upper-bound y))))))
    (make-interval lower upper)))

;;; Exercise 2.12

;;Alyssa's procedures
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100.0))))

(define (percent i)
  (* 100 (/ (width i) (center i))))

;test
(make-center-percent 5.0 10)            ;(4.5 . 5.5)
(percent (make-center-percent 5.0 10))  ;10.0

;*_tol are percentages in fraction
;prod_tol = prod_width * prod_cen / 100
;= prod_width * [ (fac1_l * fac2_l + fac1_u * fac2_u) / 2 ] / 100
;just the square brackets (= prod_cen):
;[ ( fac1_cen - fac1_cen * fac1_tol) * ( fac2_cen - fac2_cen * fac2_tol )
;+ ( fac1_cen + fac1_cen * fac1_tol) * ( fac2_cen + fac2_cen * fac2_tol ) / 2 ]
;[ fac1_cen * ( 1 - fac1_tol ) * fac2_cen * ( 1 - fac2_tol ) + fac1_cen * ( 1 + fac1_tol ) * fac2_cen * ( 1 + fac2_tol ) / 2]
;[ fac1_cen * fac2_cen * ( (1-fac1_tol)*(1-fac2_tol) + (1+fac1-tol)*(1+fac2_tol) ) / 2 ]
;[ fac1_cen * fac2_cen * ( 1 - (fac1_tol+fac2_tol) + fac1_tol*fac2_tol + 1 + (fac1_tol+fac2_tol) + fac1_tol*fac2_tol) / 2 ]
;[ fac1_cen * fac2_cen * ( 2 + 2 * fac1_tol*fac2_tol ) / 2 ]
;[ fac1_cen * fac2_cen * ( 1 + fac1_tol * fac2_tol ) ]

;;; Exercises 2.14-2.15

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(define a (make-center-percent 4 5))
(define b (make-center-percent 2 3))

(par1 a b)                              ;(1.1776357827476038 . 1.5073170731707317)
(par2 a b)                              ;(1.284320557491289 . 1.382108626198083)
(center (par1 a b))                     ;1.3424764279591677
(percent (par1 a b))                    ;12.278848386347807
(center (par2 a b))                     ;1.333214591844686
(percent (par2 a b))                    ;3.6673791790521415
;significant differnce in the resulting tolerances
(percent (div-interval a b))            ;7.988017973040449
(percent (div-interval a a))            ;9.975062344139651 even though
;a has a wider tolerance, performing an operation of it on itself
;leads to a smaller tolerance than performing an operation of it on b,
;which increases uncertainty. However, in both cases uncertainty
;increases, hence it's necessary to repeat intervals as less as
;possible - which par2 does better, using each interval only once.

;;; Exercise 2.16

;Equivalend algebraic expressions may lead to different answers
;because any uncertainty - not only intervals but also unprecise
;floating-poing numbers - may be expressed in different ways in
;algebraicly equivalent expressions. The reason is computational and
;not purely mathematical.

;It may be possible to overcome this by computing center and width
;separately by some mathematical means, while also keeping certain
;precision of floating point numbers.

