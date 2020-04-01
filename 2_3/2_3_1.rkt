#lang sicp

;;; -----------------
;;; 2.3 Symbolic Data
;;; -----------------


;;; 2.3.1 Quotation
;;; ---------------

;;; Exercise 2.53

(list 'a 'b 'c)
;(a b c)

(list (list 'george))
;((george))

(cdr '((x1 x2) (y1 y2)))
;((y1 y2))

(cadr '((x1 x2) (y1 y2)))
;(y1 y2)

(pair? (car '(a short list)))
;#f

(memq 'red '((red shoes) (blue socks)))
;#f

(memq 'red '(red shoes blue socks))
;(red shoes blue socks)

;;; Exercise 2.54

(define (equal a b)
  (cond ((and (not (pair? a)) (not (pair? b)))
         (if (eq? a b) #t #f))
        ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        (else #f))))

;test
(equal? 'f 'f)                          ;#t
(equal? 'r 'f)                          ;#f
(equal? '(a b) '(a b))                  ;#t
(equal? '(a b) '(a c))                  ;#f
(equal? '(a b) '(a b c))                ;#f

;;; Exercise 2.55

(car ''abracadabra)

;The quotation symbol ' is read as "quote", therefore the entire
;expression is read as:
;(car (quote (quote abracadabra))) which is:
;(car '(quote abracadabra)) which evaluates to quote

