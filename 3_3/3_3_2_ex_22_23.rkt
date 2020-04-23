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

;item: (<data> <ptr-to-prev.> . <ptr-to-next>)

(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty?) (null? front-ptr))
    (define (front)
      (if (empty?)
          (error "FRONT called with an empty queue" front-ptr)
          (car front-ptr)))
    (define (rear)
      (if (empty?)
          (error "REAR called with an empty queue" front-ptr)
          (car rear-ptr)))
    (define (front-insert! item)
      (let ((new-pair (cons item (cons '() front-ptr))))
        (cond ((empty?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               front-ptr)
              (else
               (set-car! (cdr front-ptr) new-pair)
               (set-front-ptr! new-pair)
               front-ptr))))
    (define (rear-insert! item)
      (let ((new-pair (cons item (cons rear-ptr '()))))
        (cond ((empty?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               front-ptr)
              (else
               (set-cdr! (cdr rear-ptr) new-pair)
               (set-rear-ptr! new-pair)
               front-ptr))))
    (define (front-delete!)
      (cond ((empty?)
             (error "FRONT-DELETE! called with an empty queue" front-ptr))
            (else
             (set-front-ptr! (cddr front-ptr))
             front-ptr)))
    (define (rear-delete!)
      (cond ((empty?)
             (error "REAR-DELETE! called with an empty queue" front-ptr))
            (else
             (set-rear-ptr! (cadr rear-ptr))
             front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'empty?) empty?)
            ((eq? m 'front) front)
            ((eq? m 'rear) rear)
            ((eq? m 'front-insert!) front-insert!)
            ((eq? m 'rear-insert!) rear-insert!)
            ((eq? m 'front-delete!) front-delete!)
            ((eq? m 'rear-delete!) rear-delete!)
            (else (error "Invalid queue operation" m))))
    dispatch))

(define (empty-deque? dq) ((dq 'empty?)))
(define (front-deque dq) ((dq 'front)))
(define (rear-deque dq) ((dq 'rear)))
(define (front-insert-deque! dq item) ((dq 'front-insert!) item))
(define (rear-insert-deque! dq item) ((dq 'rear-insert!) item))
(define (front-delete-deque! dq) ((dq 'front-delete!)))
(define (rear-delete-deque! dq) ((dq 'rear-delete!)))

;test
(define dq (make-deque))
(empty-deque? dq)                       ;#t
(front-deque dq)                        ;; FRONT called with an empty queue ()
(rear-deque dq)                         ;; REAR called with an empty queue ()
(front-insert-deque! dq 1)              ;(1 ())
(front-insert-deque! dq 2)              ;#0=(2 () 1 #0#)
(front-deque dq)                        ;2
(rear-deque dq)                         ;1
(rear-insert-deque! dq 3)               ;#0=(2 () . #1=(1 #0# 3 #1#))
(rear-deque dq)                         ;3
(front-delete-deque! dq)                ;#0=(1 (2 () . #0#) 3 #0#)
(front-deque dq)                        ;1
(rear-delete-deque! dq)                 ;#0=(1 (2 () . #0#) 3 #0#)
(rear-deque dq)                         ;1

;;TODO: printing method. Or at least a mutating procedure should return
;;something more meaningful. Also, deleting should manipulate the pointers of
;;the neighboring item.
