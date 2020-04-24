#lang sicp

;;; ------------------------------
;;; 3.3 Modeling with Mutable Data
;;; ------------------------------


;;; 3.3.3 Representing Tables
;;; -------------------------

;;; Exercise 3.24

(define (make-table same-key?)
  ;; all that is needed is an internal assoc that uses same-key?
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;test
(define key-length-table (make-table (lambda (k1 k2)
                                       (if (and (list? k1) (list? k2))
                                           (= (length k1) (length k2))
                                           (equal? k1 k2)))))
;(a table where keys that are lists are the same key if they are the same length)
((key-length-table 'insert-proc!) '(length 2) '(and length 3) 'success!)
((key-length-table 'lookup-proc) '(also 2) '(and also 3)) ;success!
((key-length-table 'insert-proc!) 'normal-key 'another 'secret)
((key-length-table 'lookup-proc) 'just 'a-key) ;#f
((key-length-table 'lookup-proc) '(list with other length) '(yet another one)) ;#f
((key-length-table 'lookup-proc) 'normal-key 'another)                         ;secret
