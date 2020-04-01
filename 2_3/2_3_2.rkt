#lang sicp

;;; -----------------
;;; 2.3 Symbolic Data
;;; -----------------


;;; 2.3.2 Example: Symbolic Differentiation
;;; ---------------------------------

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp)
;;          (if (same-variable? exp var) 1 0))
;;         ((sum? exp)
;;          (make-sum (deriv (addend exp) var)
;;                    (deriv (augend exp) var)))
;;         ((product? exp)
;;          (make-sum
;;           (make-product (multiplier exp)
;;                         (deriv (multiplicand exp) var))
;;           (make-product (deriv (multiplier exp) var)
;;                         (multiplicand exp))))
;;         (else
;;          (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (addend s) (cadr s))

;; (define (augend s) (caddr s))

(define (multiplier p) (cadr p))

;; (define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;;; Exercise 2.56


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp) (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        (else (list '** base exp))))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

;test
;(deriv '(+ (** x 3) (* 2 (* x y))) 'x)  ;(+ (* 3 (** x 2)) (* 2 y))

;;; Exercise 2.57

;(+ a b c d)
;augend: (+ b c d)

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

;test
(deriv '(* x y (+ x 3)) 'x)             ;(+ (* x y) (* y (+ x 3)))

;;; Exercise 2.58

;a.
;Using new procedures for everything

;Identical to deriv except for using infix- versions
(define (infix-deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((infix-sum? exp)
         (make-infix-sum (infix-deriv (infix-addend exp) var)
                         (infix-deriv (infix-augend exp) var)))
        ((infix-product? exp)
         (make-infix-sum
          (make-infix-product (infix-multiplier exp)
                              (infix-deriv (infix-multiplicand exp) var))
          (make-infix-product (infix-deriv (infix-multiplier exp) var)
                              (infix-multiplicand exp))))
        ((infix-exponentiation? exp)
         (make-infix-product
          (make-infix-product (infix-exponent exp)
                              (make-infix-exponentiation (infix-base exp) (- (infix-exponent exp) 1)))
          (infix-deriv (infix-base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (infix-sum? x)
  (and (pair? x) (not (null? (cdr x)))
       (eq? (cadr x) '+)
       (not (null? (cddr x)))))

(define (infix-product? x)
  (and (pair? x) (not (null? (cdr x)))
       (eq? (cadr x) '*)
       (not (null? (cddr x)))))

(define (infix-exponentiation? x)
  (and (pair? x) (not (null? (cdr x)))
       (eq? (cadr x) '**)
       (not (null? (cddr x)))))

(define (infix-addend s) (car s))

(define (infix-augend s) (caddr s))

(define (infix-multiplier p) (car p))

(define (infix-multiplicand p) (caddr p))

(define (infix-base e) (car e))

(define (infix-exponent e) (caddr e))

(define (make-infix-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-infix-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (make-infix-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        (else (list base '** exp))))

;test
(infix-deriv '((2 * (x ** 2)) + ((4 * x) + 7)) 'x) ;((2 * (2 * x)) + 4)

;b
;Using new procedures for everything

;Identical to deriv except for using std- versions
(define (std-deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((std-sum? exp)
         (make-std-sum (std-deriv (std-addend exp) var)
                         (std-deriv (std-augend exp) var)))
        ((std-product? exp)
         (make-std-sum
          (make-std-product (std-multiplier exp)
                              (std-deriv (std-multiplicand exp) var))
          (make-std-product (std-deriv (std-multiplier exp) var)
                              (std-multiplicand exp))))
        ((std-exponentiation? exp)
         (make-std-product
          (make-std-product (std-exponent exp)
                              (make-std-exponentiation (std-base exp) (- (std-exponent exp) 1)))
          (std-deriv (std-base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (std-sum? x)
  (if (and (pair? x) (not (null? (cdr x))))
      (or (infix-sum? x) (std-sum? (cdr x)))
      #f))

(define (std-product? x)
  (if (and (pair? x) (not (null? (cdr x))) (not (std-sum? x)))
      (or (infix-product? x) (std-product? (cdr x)))
      #f))

(define (std-exponentiation? x)
  (if (and (pair? x) (not (null? (cdr x))) (not (std-product? x)) (not (std-sum? x)))
      (or (infix-exponentiation? x) (std-exponentiation? (cdr x)))
      #f))

;Tells what operation an expression has, or #f when there isn't any
;(thus a simple variable returns #f)
(define (tell-operation x)
  (cond ((std-sum? x) '+)
        ((std-product? x) '*)
        ((std-exponentiation? x) '**)
        (else #f)))

(define (free-single-item l)
  (if (null? (cdr l))
      (car l)
      l))

;Makes a list of two items: all items before op and all items after,
;either as lists or single items
(define (separate l op)
  (define (iter l r)
    (if (eq? (car r) op)
        (list (free-single-item l) (free-single-item (cdr r)))
        (iter (append l (list (car r))) (cdr r))))
  (iter '() l))

;All the selectors (especially for multiplication and exponentiation)
;should obviously be used only after the expression was shown to be of
;the relevant type - which deriv exactly does.

(define (std-addend s)
  (car (separate s '+)))

(define (std-augend s)
  (cadr (separate s '+)))

(define (std-multiplier p)
  (car (separate p '*)))

(define (std-multiplicand p)
  (cadr (separate p '*)))

(define (std-base e)
  (car (separate e '**)))

(define (std-exponent e)
  (cadr (separate e '**)))

(define (std-solve x)
  (if (not (pair? x))
      x
      (cond ((std-sum? x)
             (let ((parts (separate x '+))) (make-std-sum (car parts) (cadr parts))))
            ((std-product? x)
             (let ((parts (separate x '*))) (make-std-product (car parts) (cadr parts))))
            ((std-exponentiation? x)
             (let ((parts (separate x '**))) (make-std-exponentiation (car parts) (cadr parts)))))))

;When there is no operation returns 10 - useful for the connect procedure
(define (operation->order op)
  (cond ((eq? op '**) 1)
        ((eq? op '*) 2)
        ((eq? op '+) 3)
        (else 10)))

;Connects two expressions
(define (connect x y op)
  (let ((order-x (operation->order (tell-operation x)))
        (order-y (operation->order (tell-operation y)))
        (order-op (operation->order op)))
    (let ((left (if (or (> order-x order-op) (eq? op '**)) ;exponentiation always keeps parenthesis
                    (list x)
                    x))
          (op-and-right (if (or (> order-y order-op) (eq? op '**))
                            (list op y)
                            (cons op y))))
      (append left op-and-right))))

(define (make-std-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (connect (std-solve a1) (std-solve a2) '+))))

(define (make-std-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (connect (std-solve m1) (std-solve m2) '*))))

(define (make-std-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((and (number? base) (number? exp)) (expt base exp))
        (else (connect (std-solve base) (std-solve exp) '**))))

;test
(std-deriv '(3 * x ** 3 + 2 * x ** 2 + 5 * x + 5) 'x) ;(3 * 3 * x ** 2 + 2 * 2 * x + 5)

;Only thing missing is solving all redundancies after derivation. Is
;it possible without manipulating std-deriv?
