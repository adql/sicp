#lang racket

;;; -----------------------------------
;;; 2.5 Systems with Generic Operations
;;; -----------------------------------


;;; 2.5.2 Combining Data of Different Types
;;; ---------------------------------------

;;; Cleaned up summary of 2.5.1 + modification for ex. 2.81

;Implementation of put and get for this chapter
;from http://community.schemewiki.org/?sicp-ex-2.73
(define *the-table* (make-hash));make THE table
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get

(define (square x) (* x x))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

;Modified in exercise 2.81
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2) (not (eq? (car type-tags) (cadr type-tags))))
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No coercion track for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)
(install-polar-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x)
         (= x 0)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? a b)
    (and (= (numer a) (numer b))
         (= (denom a) (denom b))))
  (define (=zero? x)
    (= (numer x) 0))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)  

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ? z1 z2)
    ;This is necessary to avoid floating number impreciseness
    (or (and (= (real-part z1) (real-part z2))
             (= (imag-part z1) (imag-part z2)))
        (and (= (magnitude z1) (magnitude z2))
             (= (angle z1) (angle z2)))))
  (define (=zero? z)
    ;Again, to avoid impreciseness when converting from rectangular
    ;representation
    (< (abs (magnitude z)) 0.0001))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (equ? a b)
  (if (eq? (type-tag a) (type-tag b))
      (apply-generic 'equ? a b)
      #f))

(define (=zero? x) (apply-generic '=zero? x))

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

;;; 2.5.2 Combining Data of Different Types

;;Implementing the coercion table
(define *coercion-table* (make-hash))
(define (put-coercion key1 key2 value) (hash-set! *coercion-table* (list key1 key2) value));put 
(define (get-coercion key1 key2) (hash-ref *coercion-table* (list key1 key2) #f));get 

;;; Coercion

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

;;; Exercise 2.81

;a. This would result in infinite loop, since apply-generic will be called again with the same arguments in the same types.

;b. It works correctly since it simply fails to find a procedure just as it fails in any other case (that is, arguments of different types) with an error message.

;c. Modified above

;test
;(apply-generic 'non-proc (attach-tag 'type1 'a) (attach-tag 'type1 'b)) ;Error without trying to coerce
(;apply-generic 'non-proc (attach-tag 'type1 'a) (attach-tag 'type2 'b)) ;Error after trying to coerce

;; ;;; Exercise 2.82

;; (define (all-eq? args)
;;   (cond ((or
;;           (null? args)
;;           (not (pair? args))
;;           (null? (cdr args))) #t)
;;         ((eq? (car args) (cadr args))
;;          (all-eq? (cdr args)))
;;         (else #f)))

;; (define (no-false? args)
;;   (cond ((null? args) #t)
;;         ((eq? (car args) #f) #f)
;;         (else (no-false? (cdr args)))))

;; (define (apply-generic op . args)
;;   (define (iter-try-coerce type-tags args)
;;     (if (null? type-tags) #f
;;         (let ((target-type (car type-tags)))
;;           (let ((coers (map (lambda (arg)
;;                               (if (eq? (type-tag arg) target-type)
;;                                   #t
;;                                   (get-coercion (type-tag arg) target-type)))
;;                             args)))
;;             (if (no-false? coers)
;;                 (map (lambda (f arg)
;;                        (if (eq? f #t)
;;                            arg
;;                            (f arg))) coers args)
;;                 (iter-try-coerce (cdr type-tags) args))))))
;;   (let ((type-tags (map type-tag args)))
;;     (let ((proc (get op type-tags)))
;;       (if proc
;;           (apply proc (map contents args))
;;           (if (and (> (length args) 1) (not (all-eq? type-tags)))
;;               (let ((coerced-args (iter-try-coerce type-tags args)))
;;                 (if coerced-args
;;                     (apply apply-generic op coerced-args)
;;                     (error "No method for these types - can't coerce arguments"
;;                            (list op type-tags))))
;;               (error "No method for these types"
;;                      (list op type-tags)))))))

;; ;test
;; ;Since there are currently no operations installed with more than two arguments,
;; ;the tests will rely on failing to find an operation (but succeeding to coerce
;; ;where it's possible)
;; ;Two arguments of the same type (should succeed)
;; (apply-generic 'add (make-rational 1 2) (make-rational 3 4) ) ;'(rational 5 . 4)
;; ;Two arguments of different type (should succeed since this coercion has been
;; ;installed)
;; (apply-generic 'add 4 (make-complex-from-real-imag 3 4)) ;'(complex rectangular 7 . 4)
;; ;Three arguments of same type (should fail to find operation)
;; (apply-generic 'add 1 2 3) ; No method for these types (add (scheme-number scheme-number scheme-number))
;; ;Three arguments of different types (should fail to coerce them)
;; (apply-generic 'add 1 2 (make-rational 1 2)) ; No method for these types - can't coerce arguments (add (scheme-number scheme-number rational))
