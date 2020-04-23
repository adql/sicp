#lang sicp

;;; ------------------------------
;;; 3.3 Modeling with Mutable Data
;;; ------------------------------


;;; 3.3.2 Representing Queues
;;; -------------------------

;;; Exercise 3.22

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty?) (null? front-ptr))
    (define (front)
      (if (empty?)
          (error "FRONT called with an empty queue" front-ptr)
          (car front-ptr)))
    (define (insert! item)
      (let ((new-pair (cons item '())))
        (cond ((empty?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               front-ptr)
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               front-ptr))))
    (define (delete!)
      (cond ((empty?)
             (error "DELETE! called with an empty queue" front-ptr))
            (else
             (set-front-ptr! (cdr front-ptr))
             front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'empty?) empty?)
            ((eq? m 'front) front)
            ((eq? m 'insert!) insert!)
            ((eq? m 'delete!) delete!)
            (else (error "Invalid queue operation" m))))
    dispatch))

(define (empty-queue? q) ((q 'empty?)))
(define (front-queue q) ((q 'front)))
(define (insert-queue! q item) ((q 'insert!) item))
(define (delete-queue! q) ((q 'delete!)))

;test
(define q1 (make-queue))
(insert-queue! q1 1)                    ;(1)
(front-queue q1)                        ;1
(insert-queue! q1 2)                    ;(1 2)
(delete-queue! q1)                      ;(2)
(empty-queue? q1)                       ;#f
(delete-queue! q1)                      ;()
(empty-queue? q1)                       ;#t

;;; Exercise 3.23

