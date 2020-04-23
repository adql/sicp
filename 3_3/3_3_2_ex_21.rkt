#lang sicp

;;; ------------------------------
;;; 3.3 Modeling with Mutable Data
;;; ------------------------------


;;; 3.3.2 Representing Queues
;;; -------------------------

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;;; Exercise 3.21

;;Ben's examples produce these printed results because the pointer to the head
;;of the queue, that is, the car of the pair representing the queue, points to
;;the car of a standard linked list (just like any other list), which is
;;represented as a nested list in the printed representation.

(define (print-queue q) (front-ptr q))

;test with Ben's example
(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(print-queue q1)                        ;(a b)
(delete-queue! q1)
(print-queue q1)                        ;(b)
(delete-queue! q1)
(print-queue q1)
