#lang sicp

;;; ------------------------------
;;; 3.3 Modeling with Mutable Data
;;; ------------------------------


;;; 3.3.3 Representing Tables
;;; -------------------------

;;; Exercise 3.24

(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (define (build-subtable keys value)
    (if (null? (cdr keys))
        (cons (car keys) value)
        (list (car keys) (build-subtable (cdr keys) value))))
  (define (item? subtable)
    (not (list? subtable)))
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (iter table keys)
        (let ((subtable (assoc (car keys) (cdr table))))
          (cond ((not subtable) #f)
                ((item? subtable) (cdr subtable))
                ((null? (cdr keys)) (cdr subtable))
                (else (iter subtable (cdr keys))))))
      (iter local-table keys))
    (define (insert! keys value)
      (define (iter table keys)
        (let ((subtable (assoc (car keys) (cdr table))))
          (cond ((or (not subtable) (item? subtable))
                 (set-cdr! table
                           (cons (build-subtable keys value)
                                 (cdr table))))
                ((null? (cdr keys))
                 (set-cdr! subtable value))
                (else (iter subtable (cdr keys))))))
      (iter local-table keys)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;test
(define table (make-table equal?))
((table 'insert-proc!) '(a b c) 5)      ;ok
((table 'lookup-proc) '(a b c))         ;5
((table 'lookup-proc) '(a b))           ;(c . 5)
((table 'lookup-proc) '(d))             ;#f
((table 'insert-proc!) '(a b c) 3)      ;ok
((table 'lookup-proc) '(a b c))         ;3
((table 'insert-proc!) '(c d) 10)       ;ok
((table 'lookup-proc) '(c d))           ;10
((table 'lookup-proc) '(a b c))         ;3
((table 'insert-proc!) '(c d e) 'hi)    ;ok
((table 'lookup-proc) '(c d e))         ;hi
((table 'lookup-proc) '(c d))           ;(e . hi)
((table 'insert-proc!) '(c d f) 'joe!)  ;ok
((table 'lookup-proc) '(c d e))         ;hi
((table 'lookup-proc) '(c d))           ;((f . joe!) (e . hi))

; Of course, this implementation doesn't allow multi-arity key structure (e.g.
; having both '(a b c) and '(a b)), which would be possible but more complex to
; implement.
