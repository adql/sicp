#lang sicp

;;; ------------------------------
;;; 3.3 Modeling with Mutable Data
;;; ------------------------------


;;; 3.3.1 Mutable List Structures
;;; -----------------------------

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;;; Exercise 3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v '(a b c d))
(define w (mystery v))

;;; Exercise 3.17

(define (in? x l)
  (if (null? l)
      #f
      (or (eq? x (car l)) (in? x (cdr l)))))

(define (count-pairs x)
  (define visited '())
  (define (iter x result)
    (if (or (in? x visited) (not (pair? x)))
        result
        (begin (set! visited (cons x visited))
               (let ((car-count (iter (car x) 0))
                     (cdr-count (iter (cdr x) 0)))
                 (+ 1 car-count cdr-count result)))))
  (iter x 0))

;test
(count-pairs '(1 2 3))                  ;3
(define l1 '(1 2))
(count-pairs (list l1 l1))              ;4
(count-pairs (list l1 l1 l1))           ;5
(count-pairs (cons 1 2))                ;1

;;; Exercise 3.18

;;from 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (cycle? x)
  (define (iter x1)
    (if (eq? (cdr x1) x)
        #t
        (and (> (count-pairs x1) 1) (iter (cdr x1)))))
  (iter x))

;test
(define l2 '(1 2 3))
(make-cycle l2)
(cycle? l2)                             ;#t
(cycle? '(1 2 3))                       ;#f
(cycle? (cons 1 2))                     ;#f

;test from schemewiki.org
(define t1 (cons 'a 'b)) 
(define t2 (cons t1 t1))
(cycle? t2)                             ;#f

;the challenging example from schemewiki.org fails
(define x '(a b c))
(define y '(d e f)) 
(set-car! (cdr x) y) 
(set-car! x (cdr x)) 
(set-cdr! (last-pair y) (cdr y))
(cycle? x)                              ;#f

