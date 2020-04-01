#lang sicp

;;; ------------------------------------------
;;; Hierarchical Data and the Closure Property
;;; ------------------------------------------


;;; Hierarchial Structures
;;; ----------------------

;;; Exercise 2.24

(list 1 (list 2 (list 3 4)))

;;; Exercise 2.25

;(1 3 (5 7) 9)
(car(cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
;((7))
(car (car (list (list 7))))
;(1 (2 (3 (4 (5 (6 7))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))

;;; Exercise 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

;(append x y): (1 2 3 4 5 6)
(append x y)
;(cons x y): ((1 2 3) 4 5 6)
(cons x y)
;(list x y): ((1 2 3) (4 5 6))
(list x y)

;;; Exercise 2.27

;from 2.18
(define (reverse l)
  (define (iter l result)
    (if (null? l)
        result
        (iter (cdr l) (cons (car l) result))))
  (iter l '()))

(define (deep-reverse l)
  (define (iter l result)
    (cond ((null? l) result)
          ((pair? (car l)) (iter (cdr l) (cons (deep-reverse (car l)) result)))
          (else (iter (cdr l) (cons (car l) result)))))
  (iter l '()))

;test
(define x2 (list (list 1 2) (list 3 4)))
x                                       ;((1 2) (3 4))
(reverse x2)                             ;((3 4) (1 2))
(deep-reverse x)                        ;((4 3) (2 1))

;;; Exercise 2.28

(define (fringe t)
  (define (iter t result)
    (cond ((null? t) result)
          ((pair? (car t)) (iter (cdr t) (append result (fringe (car t)))))
          (else (iter (cdr t) (append result (list (car t)))))))
  (iter t '()))

;test
x2                                      ;((1 2) (3 4))
(fringe x2)                             ;(1 2 3 4)
(fringe (list x2 x2))                   ;(1 2 3 4 1 2 3 4)

;;; Exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;a.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))


;test
(define test-mobile (make-mobile (make-branch 1 10)
                                 (make-branch 1 (make-mobile (make-branch 1 (make-mobile (make-branch 1 10)
                                                                                         (make-branch 1 5)))
                                                             (make-branch 1 (make-mobile (make-branch 1 (make-mobile (make-branch 1 10)
                                                                                                                     (make-branch 1 5)))
                                                                                         (make-branch 1 (make-mobile (make-branch 1 5)
                                                                                                                     (make-branch 1 10)))))))))
(branch-structure (right-branch (branch-structure (left-branch (branch-structure (right-branch (branch-structure (right-branch test-mobile)))))))) ;5

;b.

(define (total-weight mobile)
  (define (branch-weight branch)
    (let ((structure (branch-structure branch)))
      (if (not (pair? structure))
          structure
          (total-weight structure))))
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

;test
(total-weight test-mobile)              ;55

;c.

;makes sense to have a separate procedure for this
(define (torque branch)
  (let ((structure (branch-structure branch)))
    (* (branch-length branch)
       (if (pair? structure)
           (total-weight structure)
           structure))))

(define (balanced? mobile)
  (define (branches-balanced? mobile)
    (=  (torque (left-branch mobile)) (torque (right-branch mobile))))
  (if (not (branches-balanced? mobile))
      #f
      (let ((left (branch-structure (left-branch mobile)))
            (right (branch-structure (right-branch mobile))))
        (cond ((and (pair? left) (not (balanced? left))) #f)
              ((and (pair? right) (not (balanced? right))) #f)
              (else #t)))))

;test
(define balanced-mobile (make-mobile (make-branch 5 20)
                                     (make-branch 2 (make-mobile (make-branch 1 (make-mobile (make-branch 1 30)
                                                                                             (make-branch 3 10)))
                                                                 (make-branch 4 10)))))
(balanced? test-mobile)                 ;#f
(balanced? balanced-mobile)             ;#t

;d.

;The only needed change is of right-branch and branch-structure to
;take the cdr directly without car (since the cdr doesn't give a
;list).

;;; Exercise 2.30

(define (square x) (* x x))

(define (square-tree-directly tree)
  (define (iter tree result)
    (if (null? tree) result
        (let ((next (car tree)))
          (if (pair? next)
              (iter (cdr tree) (append result (list (iter next nil))))
              (iter (cdr tree) (append result (list (square next))))))))
  (iter tree '()))

;test
(define test-tree (list 1 (list 2 3 (list (list 4 5) 6))))
test-tree                               ;(1 (2 3 ((4 5) 6)))
(square-tree-directly test-tree)        ;(1 (4 9 ((16 25) 36)))

(define (square-tree-map tree)
  (map (lambda (branch)
         (if (pair? branch)
             (square-tree-map branch)
             (square branch)))
       tree))

;test
(square-tree-map test-tree)             ;(1 (4 9 ((16 25) 36)))

;;; Exercise 2.31

(define (tree-map proc tree)
  (map (lambda (branch)
         (if (pair? branch)
             (tree-map proc branch)
             (proc branch)))
       tree))

;test
(define (square-tree tree) (tree-map square tree))
(square-tree test-tree)                 ;(1 (4 9 ((16 25) 36)))

;;; Exercise 2.32

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (t)
                        (cons (car s) t)) rest)))))

;test
(subsets (list 1 2 3))                  ;(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
