#lang racket

;;; -----------------------------------
;;; 2.5 Systems with Generic Operations
;;; -----------------------------------

;Import the most recently completed exercise from 2.5.2
(require "2_5_2_ex_83-84.rkt") 


;;; 2.5.3 Example: Symbolic Algebra
;;; -------------------------------

;from 2.3.2

;;; Arithmetic on polynomials

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;; <_procedures `same-variable?' and `variable?' from section 2.3.2_>
  (define (variable? x) (symbol? x))  
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  ;; <_procedures `adjoin-term' ... `coeff' from text below_>
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  ;; <_procedures used by `add-poly'_>
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  ;; <_procedures used by `mul-poly'_>
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))

  ;; Exercise 2.87
  (define (=zero-poly? p) (empty-termlist? (term-list p)))
  ;(since adjoin-term doesn't adjoin null-terms this is enough)
  (put '=zero? '(polynomial) =zero-poly?)

  ;; Exercise 2.88
  (define (neg-poly p)
    (make-poly (variable p) (neg-terms (term-list p))))
  (define (neg-terms L)
    (mul-term-by-all-terms (make-term 0 -1) L))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (sub-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var -- SUB-POLY")))
  (define (sub-terms L1 L2)
    (add-terms L1 (neg-terms L2)))

  (put 'neg '(polynomial)
       (lambda (p) (tag (neg-poly p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))

  ;; Exercise 2.91
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((division (div-terms (term-list p1) (term-list p2))))
          (list (make-poly (car division))
                (make-poly (cadr division))))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (div-terms (sub-terms L1
                                             (mul-term-by-all-terms (make-term new-o new-c) L2))
                                  L2)
                       ))
                  (list (adjoin-term (make-term new-o new-c) (car rest-of-result))
                        (cadr rest-of-result))))))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  'done)

;;; Representing term lists

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(install-polynomial-package)

;;; Exercise 2.87

;test
(=zero? (make-polynomial 'x '()))       ;#t
(=zero? (make-polynomial 'x '((2 2) (1 3)))) ;#f

;;; Exercise 2.88

(define (neg x) (apply-generic 'neg x))

(define (sub x y) (apply-generic 'sub x y))

;test
(sub (make-polynomial 'x '((3 4) (1 2) (0 4)))
     (make-polynomial 'x '((3 2) (2 4) (0 1)))) ;'(polynomial x (3 2) (2 -4) (1 2) (0 3))

;;; Exercise 2.91

;test
(div (make-polynomial 'x '((4 3) (3 5) (2 1) (1 5) (0 3)))
     (make-polynomial 'x '((2 5) (1 3) (0 5))))
