#lang sicp

;;; -----------------
;;; 2.3 Symbolic Data
;;; -----------------


;;; 2.3.3 Example: Representing Sets
;;; --------------------------------

;;; Sets as unordered lists

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;;; Exercise 2.59

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1) (adjoin-set (car set1) set2))))

;test
(union-set '(a b c) '(c d e))           ;(b a c d e)

;;; Exercise 2.60

;This is identical
(define dupl-element-of-set? element-of-set?)

(define dupl-adjoin-set cons)

(define dupl-union-set append)

;Also identical, including the call to element-of-set?
(define dupl-intersection-set intersection-set)

;test
(dupl-element-of-set? 'a '(a b a b c))  ;#t
(dupl-adjoin-set 'a '(a b c))           ;(a a b c)
(dupl-union-set '(a b c) '(b c d))      ;(a b c b c d)
(dupl-intersection-set '(a b a b c) '(b c b c d)) ;(b b c)

;This simplifies adjoin-set and union-set by avoiding calling
;element-of-set?, thus reducing them to elementary cons and append
;respectively. However, this doesn't change intersection-set which
;still has to call element-of-set n^2 times, this time possibly with
;larger n. It would be useful for applications that apply a lot of
;unions but not many intersections, such as maintaining a growing list
;of elements.

;Actually it's always better to use normal lists, unless
;unrepetative sets are explicitely needed (as explained in Schemewiki)

;;; Sets as ordered lists

(define (ordered-element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (ordered-intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;;; Exercise 2.61

(define (ordered-adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((> x (car set)) (cons (car set) (ordered-adjoin-set x (cdr set))))
        (else (cons x set))))

;test
(ordered-adjoin-set 4 '(1 3 6))         ;(1 3 4 6)
(ordered-adjoin-set 6 '(1 2 4))         ;(1 2 4 6)
(ordered-adjoin-set 3 '(4 6 7))         ;(3 4 6 7)
(ordered-adjoin-set 4 '(1 4 6))         ;(1 4 6)

;;; Exercise 2.62

(define (ordered-union-set set1 set2)
  (if (null? set2)
      set1
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (ordered-union-set (cdr set1) (cdr set2))))
              ((< x1 x2)
               (cons x1 (ordered-union-set set2 (cdr set1))))
              (else
               (cons x2 (ordered-union-set set1 (cdr set2))))))))

;test
(ordered-union-set '(1 2 3) '(2 3 4))   ;(1 2 3 4)
(ordered-union-set '(2 3 4) '(1 2 3))   ;(1 2 3 4)
(ordered-union-set '(1 2 3) '(4 5 6))   ;(1 2 3 4 5 6)
(ordered-union-set '(1 2 3) '(1 2 3))   ;(1 2 3)

;;; Sets as binary trees

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (bt-element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (bt-element-of-set? x (left-branch set)))
        ((> x (entry set))
         (bt-element-of-set? x (right-branch set)))))

(define (bt-adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (bt-adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (bt-adjoin-set x (right-branch set))))))

;;; Exercise 2.63

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;testing with the trees from figure 2.16
(define tree-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define tree-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define tree-3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
(tree->list-1 tree-1)                   ;(1 3 5 7 9 11)
(tree->list-2 tree-1)                   ;(1 3 5 7 9 11)
(tree->list-1 tree-2)                   ;(1 3 5 7 9 11)
(tree->list-2 tree-2)                   ;(1 3 5 7 9 11)
(tree->list-1 tree-3)                   ;(1 3 5 7 9 11)
(tree->list-2 tree-3)                   ;(1 3 5 7 9 11)
;All the same, as expected.

;Both versions call recursion on both branches on the tree. However, version 2 calls recursion on the right branch within the call to recursion on the left branch. Version 1 does the recursions parallelly. It is therefore reasonable to expect O(n) in version 1 and O(n^2) in version 2.
;No... version two is indeed slower but the procedures have 

;;; Exercise 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(list->tree '(1 3 5 7 9 11))
;   5
;  / \
; 1   9
; \   /\
;  3 7  11

;;; Exercise 2.65

(define (bt-union-set tree1 tree2)
  (let ((l1 (tree->list-2 tree1))
        (l2 (tree->list-2 tree2)))
    (list->tree (ordered-union-set l1 l2))))

;test
(define tree-4 (list->tree '(1 3 4 6 8 10)))
(define tree-5 (list->tree '(1 2 5 6 7 11)))
(bt-union-set tree-4 tree-5)
;         5
;       /   \
;      2     8
;     /\     /\
;    1  3   6  10
;        \   \   \
;         4   7   11

(define (bt-intersection-set tree1 tree2)
  (let ((l1 (tree->list-2 tree1))
        (l2 (tree->list-2 tree2)))
    (list->tree (ordered-intersection-set l1 l2))))

;test
(bt-intersection-set tree-4 tree-5)
;  1
;   \
;    6

;Both procedures sequentially use sub-procedures with O(n), which sums
;to 4*O(n), which is same order as O(n).

;;; Sets and information retrieval

;;; Exercise 2.66

(define (lookup given-key set-of-records)
  (let ((current-entry (entry set-of-records)))
    (cond ((null? set-of-records) false)
          ((= given-key (key current-entry))
           current-entry)
          ((< given-key (key current-entry))
           (lookup given-key (left-branch set-of-records)))
          ((> given-key (key current-entry))
           (lookup given-key (right-branch set-of-records))))))

;test using simple record implementation
(define key car)
(define value cadr)
(define test-set-of-records
  '((5 "Amir") ((3 "Yossy") ((2 "Serge") () ())
                            ((4 "Landa") () ()))
               ((7 "Boaz") ()
                           ((9 "Liad") () ()))))
(value (lookup 7 test-set-of-records))
(value (lookup 4 test-set-of-records))
