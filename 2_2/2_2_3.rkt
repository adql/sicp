#lang sicp

;;; ------------------------------------------
;;; Hierarchical Data and the Closure Property
;;; ------------------------------------------


;;; 2.2.3 Sequences as Convensional Interfaces
;;; ------------------------------------------

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;;; Exercise 2.33

;; (define (map p sequence)
;;   (accumulate (lambda (x y)
;;                 (cons (p x) y)) nil sequence))

;; (define (append seq1 seq2)
;;   (accumulate cons seq2 seq1))

;; (define (length sequence)
;;   (accumulate (lambda (next count)
;;                 (inc count)) 0 sequence))

;;; Exercise 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;testing
(horner-eval 2 (list 1 3 0 5 0 1))

;;; Exercise 2.35

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))

;test
(define test-tree (list 1 (list 2 3) 4 (list (list 5 6) 7) 8))
test-tree
(count-leaves test-tree)                ;8

;;; Exercise 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      ;; This is actually stupid, could have used car and cdr directly
      (cons (accumulate op init (map (lambda (seq) (car seq)) seqs))
            (accumulate-n op init (map (lambda (seq) (cdr seq)) seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))) ;(22 26 30)

;;; Exercise 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (mi) (dot-product mi v)) m))

;test
(define m (list (list 1 2 3) (list 2 3 4)))
m
(define v (list 1 2 3))
v
(matrix-*-vector m v) ;(14 20)

(define (transpose mat)
  (accumulate-n (lambda (mi mj)
                  (cons mi mj)) nil mat))

;test
(transpose m)                           ;((1 2) (2 3) (3 4))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mi)
           (map (lambda (nj)
                  (dot-product mi nj))
                cols))
         m)))

;test
(define n (list (list 1 2) (list 3 4) (list 5 6)))
(matrix-*-matrix m n)                   ;((22 28) (31 40))

;;; Exercise 2.38

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; (fold-right / 1 (list 1 2 3)) -> 3/2
;; (fold-left / 1 (list 1 2 3)) -> 1/6
;; (fold-right list nil (list 1 2 3)) -> (1 (2 (3 ())))
;; (fold-left list nil (list 1 2 3)) -> (((() 1) 2) 3)

;;In order for fold-left and fold-right to produce the same values for
;;any sequence, op should be commutative and associative.

;;; Exercise 2.39

(define (reverse-fr sequence)
  (fold-right (lambda (x y)
                (append y (list x))) nil sequence))

;test
(reverse-fr (list 1 2 3))               ;(3 2 1)

(define (reverse-fl sequence)
  (fold-left (lambda (x y)
               (cons y x)) nil sequence))

;test
(reverse-fl (list 1 2 3))               ;(3 2 1)

;;; Nested Mappings

;;fast-prime? from 1.2.6 (Miller-Rabin method):
(define (square x) (* x x))
(define (expmod base exp m)
  (define (non-trivial-sqrt? b sqr_remainder)
    (and (not (= b 1))
         (< b (- m 1))
         (= sqr_remainder 1)))
  (cond ((= exp 0) 1)
        ((even? exp)
         (let* ((b (expmod base (/ exp 2) m))
                (sqr_remainder (remainder (square b) m)))
           (if (non-trivial-sqrt? b sqr_remainder) 0 sqr_remainder)))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (if (< n 4294967088)
                           (- n 1) 4294967087)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))
(define (prime? n) (fast-prime? n 5))
;;;--------------------

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;;; Exercise 2.40

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 2 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)
;((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))

;;; Exercise 2.41

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (p) (append (reverse p) (list i)))
                  (unique-pairs (- i 1))))
           (enumerate-interval 3 n)))

(define (summing-triples n s)
  (define (summing? t) (= (+ (car t) (cadr t) (caddr t)) s))
  (filter summing? (unique-triples n)))

(summing-triples 6 10)                  ;((2 3 5) (1 4 5) (1 3 6))

;;; Exercise 2.42

;auxiliary to adjoin-position
(define (splice lst i)
  (define (iter l r i)
    (if (= i 0)
        (list l r)
        (iter (append l (list (car r))) (cdr r) (- i 1))))
  (iter nil lst i))

;auxiliary to safe? (technically inexistent item is also unique)
(define (unique? l item)
  (define (iter l c)
    (if (null? l)
        #t
        (if (= (car l) item)
            (if (> c 0)
                #f
                (iter (cdr l) (+ c 1)))
            (iter (cdr l) c))))
  (iter l 0))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board
  nil)

(define (adjoin-position r k rest)
  (if (null? rest)
      (list r)
      (let ((spliced (splice rest (- k 1))))
        (append (car spliced) (list r) (cadr spliced)))))

;safe? also checks the right side, thus being more general, although
;unnecessary for that particular task
(define (safe? k positions)
  (define (check-diagonal)
    (define (iter pos v-dist orig)
      (cond ((null? pos) #t)
            ((or (= (car pos) (+ orig v-dist))
                 (= (car pos) (- orig v-dist)))
             #f)
            (else (iter (cdr pos) (+ v-dist 1) orig))))
    (let ((spliced (splice positions (- k 1))))
      (let ((left (car spliced))
            (right (cdadr spliced))
            (row (caadr spliced)))
        (and (or (null? left) (iter (reverse left) 1 row))
             (or (null? right) (iter right 1 row))))))   ;complete, even though it's not necessary
  (or (null? (cdr positions)) (and (check-diagonal) (unique? positions (caadr (splice positions (- k 1)))))))

;test
(queens 5)
;; ((1 3 5 2 4)
;;  (1 4 2 5 3)
;;  (2 4 1 3 5)
;;  (2 5 3 1 4)
;;  (3 1 4 2 5)
;;  (3 5 2 4 1)
;;  (4 1 3 5 2)
;;  (4 2 5 3 1)
;;  (5 2 4 1 3)
;;  (5 3 1 4 2))

(length (queens 8))                     ;92 as expected
