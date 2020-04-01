#lang racket

;;; ----------------------------------------------
;;; 2.4 Multiple Representations for Abstract Data
;;; ----------------------------------------------


;Implementation of put and get for this chapter
;from http://community.schemewiki.org/?sicp-ex-2.73
(define *the-table* (make-hash));make THE table 
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put 
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get 

(define (square x) (* x x))

;;; 2.4.2 Tagged data
;;; -----------------

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))


;;; 2.4.3 Data-Directed Programming and Additivity
;;; ----------------------------------------------

;Bens implementation
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

;Alyssa's implementation
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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;Installing the packages
;; (install-rectangular-package)
;; (install-polar-package)

;;; Exercise 2.73

;Original ausxiliary procedures (modified versions from exercises)
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

(define (addend s) (cadr s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

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

;Original deriv procedure for testing
(define (deriv-orig exp var)
  (cond ((number? exp) 0) 
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv-orig (addend exp) var)
                   (deriv-orig (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv-orig (multiplicand exp) var))
          (make-product (deriv-orig (multiplier exp) var)
                        (multiplicand exp))))
        (else (error "unknown expression type -- DERIV" exp))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;b

(define (install-deriv-package)
  ;I don't see the need for internal abstraction of the addend,
  ;augend, etc. procedures, since the deriv procedures here only
  ;receive a list of operands which can be simply regarded as
  ;such. Furthermore, the make-sum and make-product procedures should
  ;exist in such a system on the upper level and be available for
  ;other use, hence they are also not defined here internally.
  
  ;Internal derivation procedures
  (define (deriv-sum operands var)
    (make-sum (deriv (car operands) var)
              (deriv (cadr operands) var)))
  (define (deriv-product operands var)
    (let ((multiplier (car operands))
          (multiplicand (cadr operands)))
      (make-sum
       (make-product multiplier (deriv multiplicand var))
       (make-product (deriv multiplier var) multiplicand))))

  ;Installation
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product))

;test
(install-deriv-package)
(define test-exp '(+ (* 3 (*  x x)) (* 5 x) 6))
(deriv-orig test-exp 'x)                ;'(+ (* 3 (+ x x)) 5)
(deriv test-exp 'x)                     ;'(+ (* 3 (+ x x)) 5)

;;Failed attempt

;; (define (install-deriv-package)

;;   ;; Internal definitions

;;   ;Selectors have to be redifined since they now
;;   ;receive the operands directly without the operator
;;   (define (multiplier operands) (car p))
;;   (define (multiplicand operands)
;;     (if (null? (cddr operands))
;;         (cadr operands)
;;         (cons '* (cdr operands))))
;;   (define (addend operands) (car operands))
;;   (define (augend operands)
;;     (if (null? (cddr operands))
;;         (cadr operands)
;;         (cons '+ (cdr operands))))

;;   (define (deriv-sum operands var)
;;     (make-sum (deriv (car operands) var)
;;               (deriv (cdr operands) var)))
;;   (define (deriv-product operands var)
;;     (make-sum
;;      (make-product (car operands)
;;                    (deriv (cdr operands) var))
;;      (make-product (deriv (car operands) var)
;;                    (cdr operands))))

;;   ;; Installing the definitions
;;   (put 'deriv '+ deriv-sum)
;;   (put 'deriv '* deriv-product))

;;It probably failed only because of cdring instead of cadring

;c

(define (make-exponentiation base exp)
  (cond ((= exp 0) 1)
        ((= exp 1) base)
        (else (list '** base exp))))

(define (install-exponent-deriv-rule)
  (define (deriv-exponentiation operands var)
    (let ((base (car operands))
          (exp (cadr operands)))
      (make-product (make-product exp (make-exponentiation base (- exp 1)))
                    (deriv base var))))
  (put 'deriv '** deriv-exponentiation))

;test
(install-exponent-deriv-rule)
(define test-exp-2 '(** (* 2 x) 4))
(deriv test-exp-2 'x)                   ;'(* (* 4 (** (* 2 x) 3)) 2)

;d

;This just crosses the columns and rows of the table, so each put call
;in the installation procedure should be of the form:
;(put '<type> 'deriv <deriv-procedure>)

;;; Exercise 2.74

;; FAILED ATTEMPT! (a and b work but there's no way to get c to work
;; under this implementation)

;First, I'll create two examples for divisions with their different
;implementations of the employee file and each employee's data.

;; (define division-north-personnel
;;   '((employee "Johannes Vogel"
;;               (name "Johannes Vogel"
;;                     age 38
;;                     address "Nowhere St. 4"
;;                     salary 14000))
;;     (employee "Ayala Shir"
;;               (name "Ayala Shir"
;;                     age 32
;;                     address "HaShir 5"
;;                     salary 13000))))

;; (define division-south-personnel
;;   '(("Agdar Yohav"
;;      (name "Agdar Yohav")
;;      (age 44)
;;      (address "Somewhere weird")
;;      (salary 22000))
;;     ("Noam Raviv"
;;      (name "Noam Raviv")
;;      (age 34)
;;      (address "HaBonim Scheisse 37")
;;      (salary 18000))))

;; ;a.

;; ;Installation of the division's procedures

;; (define (install-api-north)
;;   (define (get-record employee personnel)
;;     (define (search personnel)
;;       (if (null? personnel)
;;           #f
;;           (let ((current (car personnel)))
;;             (if (eq? (cadr current) employee)
;;                 (caddr current)
;;                 (search (cdr personnel))))))
;;     (search personnel))
;;   (define (get-salary record)
;;     (define (search record)
;;       (cond ((null? record) (error "Invalid record or no salary" record))
;;             ((eq? (car record) 'salary) (cadr record))
;;             (else (search (cddr record)))))
;;     (search record))

;;   (put 'get-record 'division-north get-record)
;;   (put 'get-salary 'division-north get-salary))

;; (install-api-north)

;; (define (install-api-south)
;;   ;Just trying opposite order of arguments so that the put call has to adapt
;;   (define (get-record personnel employee)
;;     (define (search personnel)
;;       (cond ((null? personnel) #f)
;;             ((eq? (caar personnel) employee) (cdar personnel))
;;             (else (search (cdr personnel)))))
;;     (search personnel))
;;   (define (get-salary record)
;;     (define (search record)
;;       (cond ((null? record) (error "Invalid record or no salary" record))
;;             ((eq? (caar record) 'salary) (cadar record))
;;             (else (search (cdr record)))))
;;     (search record))

;;   (put 'get-record 'division-south
;;        (lambda (em pers)
;;          (get-record pers em)))
;;   (put 'get-salary 'division-south get-salary))

;; (install-api-south)

;; ;Final general procedure

;; (define (get-record employee personnel division)
;;   ((get 'get-record division) employee personnel))

;; ;b

;; (define (get-salary employee personnel division)
;;   (let ((record (get-record employee personnel division)))
;;     ((get 'get-salary division) record)))

;; ;c

;; (define (find-employee-record employee personnel-files)
;;   (let ((record (get-record)))))

;;;;; A HUGE MISTAKE!
;;;;; I had to include in each personnel file an identifier for the division
;;;;; Basiclly everything has to be changed.

;;Revising...

;Using the same original personnel files, but...

(define division-north-personnel
  '((employee "Johannes Vogel"
              (name "Johannes Vogel"
                    age 38
                    address "Nowhere St. 4"
                    salary 14000))
    (employee "Ayala Shir"
              (name "Ayala Shir"
                    age 32
                    address "HaShir 5"
                    salary 13000))))

(define division-south-personnel
  '(("Agdar Yohav"
     (name "Agdar Yohav")
     (age 44)
     (address "Somewhere weird")
     (salary 22000))
    ("Noam Raviv"
     (name "Noam Raviv")
     (age 34)
     (address "HaBonim Scheisse 37")
     (salary 18000))))

;a.

;... encapsulating each personnel file in a list with division
;identifier (thus the original file is left unchanged within the new
;record, as opposed to adding the type into it)
(define (mark-division division data)
  (list division data))

;Putting it all in one list

(define general-personnel
  (list (mark-division 'division-south division-south-personnel)
        (mark-division 'division-north division-north-personnel)))

;Installation of the division's procedures

(define (install-api-north)
  (define (get-record employee personnel)
    (define (search personnel)
      (if (null? personnel)
          #f
          (let ((current (car personnel)))
            (if (eq? (cadr current) employee)
                (caddr current)
                (search (cdr personnel))))))
    (search personnel))
  (define (get-salary record)
    (define (search record)
      (cond ((null? record) (error "Invalid record or no salary" record))
            ((eq? (car record) 'salary) (cadr record))
            (else (search (cddr record)))))
    (search record))

  (put 'get-record 'division-north get-record)
  (put 'get-salary 'division-north get-salary))

(install-api-north)

(define (install-api-south)
  ;Just trying opposite order of arguments so that the put call has to adapt
  (define (get-record personnel employee)
    (define (search personnel)
      (cond ((null? personnel) #f)
            ((eq? (caar personnel) employee) (cdar personnel))
            (else (search (cdr personnel)))))
    (search personnel))
  (define (get-salary record)
    (define (search record)
      (cond ((null? record) (error "Invalid record or no salary" record))
            ((eq? (caar record) 'salary) (cadar record))
            (else (search (cdr record)))))
    (search record))

  (put 'get-record 'division-south
       (lambda (em pers)
         (get-record pers em)))
  (put 'get-salary 'division-south get-salary))

(install-api-south)

;Final general procedure

(define (get-record employee personnel)
  (let ((op (get 'get-record (car personnel))))
    (if op
        (op employee (cadr personnel))
        (error "Invalid personnel file -- GET-RECORD" personnel))))

;test
(get-record "Noam Raviv" (car general-personnel))
(get-record "Ayala Shir" (cadr general-personnel))

;b

(define (get-salary employee personnel)
  (let ((record (get-record employee personnel)))
    ((get 'get-salary (car personnel)) record)))

;test
(get-salary "Johannes Vogel" (cadr general-personnel)) ;14000

;c

(define (find-employee-record employee general-personnel)
  (if (null? general-personnel)
      #f
      (let ((record (get-record employee (car general-personnel))))
        (if record
            record
            (find-employee-record employee (cdr general-personnel))))))

;test
(find-employee-record "Ayala Shir" general-personnel)

;d

;As with the older records, the new personnel file would have to be
;encapsulated in a list (<division-tag> <personnel>).

;;; Message Passing

(define (make-from-real-imag-mp x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;;; Exercise 2.75

(define (make-from-mag-ang-mp mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* mag (cos ang)))
          ((eq? op 'imag-part)
           (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;test
((make-from-mag-ang-mp 10 (/ pi 6)) 'imag-part) ;4.9999
