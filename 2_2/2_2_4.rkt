#lang racket

;;; ------------------------------------------
;;; Hierarchical Data and the Closure Property
;;; ------------------------------------------

(#%require sicp-pict)

;;; 2.2.4 Example: A Picture Language
;;; ---------------------------------

;; In this chapter I avoided overriding the procedures provided by
;; #lang sicp since the basic drawing mechanism cannot be redefined.

;; (define (right-split painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((smaller (right-split painter (- n 1))))
;;         (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;; (define (square-limit painter n)
;;   (let ((quarter (corner-split painter n)))
;;     (let ((half (beside (flip-horiz quarter) quarter)))
;;       (below (flip-vert half) half))))

;;; Exercise 2.44

;; (define (up-split painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((smaller (up-split painter (- n 1))))
;;         (below painter (beside smaller smaller)))))

;;; Higher-order operations

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define flipped-pairs
  (square-of-four identity flip-vert identity flip-vert))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                   rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;;; Exercise 2.45

(define (split dir1 dir2)
  (define (splitter painter n)
    (if (= n 0)
        painter
        (let ((smaller (splitter painter (- n 1))))
          (dir1 painter (dir2 smaller smaller)))))
  splitter)

;test
(define right-split (split beside below))
(define up-split (split below beside))
(paint (right-split einstein 5))
(paint (up-split einstein 5))

;;; Frames

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;;; Exercise 2.46

(define (my-make-vect x y)
  (if (and (number? x) (number? y))
      (cons x y)
      (error "Please provide two numerals")))

;doing it right
(define (is-vect? v)
  (if (and (pair? v) (number? (car v)) (number? cdr v))
      #t
      #f))

(define (xcor-vect v)
  (if (is-vect? v)
      (car v)
      (error "Not a vector")))

(define (ycor-vect v)
  (if (is-vect? v)
      (cdr v)
      (error "Not a vector")))

(define (add-vect v1 v2)
  (my-make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (my-make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

(define (sub-vect v1 v2)
  (add-vect v1 (scale-vect -1 v2)))

;;; Exercise 2.47

;; (define (make-frame origin edge1 edge2)
;;   (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
;; (define (edge2-frame frame)
;;   (caddr frame))

;test
;; (define test-frame (make-frame (make-vect 1 2) (make-vect 2 6) (make-vect 7 2)))
;; (origin-frame test-frame)               ;(1 . 2)
;; (edge1-frame test-frame)                ;(2 . 6)
;; (edge2-frame test-frame)                ;(7 . 2)

(define (my-make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

;the difference is only with edge2
(define (edge2-frame frame)
  (cddr frame))

;test
(define test-frame (my-make-frame (my-make-vect 1 2) (my-make-vect 2 6) (my-make-vect 7 2)))
(origin-frame test-frame)               ;(1 . 2)
(edge1-frame test-frame)                ;(2 . 6)
(edge2-frame test-frame)                ;(7 . 2)

;;; Painters

;This can't be used in Racket's SICP because there is no
;draw-line procedure

;; (define (segments->painter segment-list)
;;   (lambda (frame)
;;     (for-each
;;      (lambda (segment)
;;        (draw-line
;;         ((frame-coord-map frame) (start-segment segment))
;;         ((frame-coord-map frame) (end-segment segment))))
;;      segment-list)))

;;; Exercise 2.48

(define (my-make-segment v1 v2)
  (if (and (is-vect? v1) (is-vect? v2))
      (cons v1 v2)
      (error "Please provide two vectors")))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

;;; Exercise 2.49

(define outline-painter
  (segments->painter
   (list (make-segment (make-vect 0.0 0.0) (make-vect 0.0 1.0))
         (make-segment (make-vect 0.0 1.0) (make-vect 1.0 1.0))
         (make-segment (make-vect 1.0 1.0) (make-vect 1.0 0.0))
         (make-segment (make-vect 1.0 0.0) (make-vect 0.0 0.0)))))

(define x-painter
  (segments->painter
   (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
         (make-segment (make-vect 1.0 0.0) (make-vect 0.0 1.0)))))

(define diamond-painter
  (segments->painter
   (list (make-segment (make-vect 0.0 0.5) (make-vect 0.5 1.0))
         (make-segment (make-vect 0.5 1.0) (make-vect 1.0 0.5))
         (make-segment (make-vect 1.0 0.5) (make-vect 0.5 0.0))
         (make-segment (make-vect 0.5 0.0) (make-vect 0.0 0.5)))))
