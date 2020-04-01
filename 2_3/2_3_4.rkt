#lang sicp

;;; -----------------
;;; 2.3 Symbolic Data
;;; -----------------


;;; 2.3.4 Example: Huffman Encoding Trees
;;; -------------------------------

;;; Representing Huffman trees

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;;; The decoding procedure

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;;; Sets of weighted elements

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

;;; Exercise 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)     ;(A D A B B C A)

;;; Exercise 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;From 2.3.3
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (encode-symbol symbol tree)
  (define (in-branch? branch)
    (element-of-set? symbol (symbols branch)))
  (define (build-code code tree)
    (if (leaf? tree)
        code
        (let ((bit (if (in-branch? (left-branch tree)) 0 1)))
          (let ((next-branch (if (= bit 0)
                                 (left-branch tree)
                                 (right-branch tree))))
            (build-code (append code (list bit)) next-branch)))))
  (if (not (in-branch? tree))
      (error "bad symbol -- ENCODE-SYMBOL" symbol)
      (build-code '() tree)))

;test
(encode-symbol 'C sample-tree)          ;(1 1 1)
sample-message                          ;(0 1 1 0 0 1 0 1 0 1 1 1 0)
(encode '(A D A B B C A) sample-tree)   ;(0 1 1 0 0 1 0 1 0 1 1 1 0)

;;; Exercise 2.69

(define (generate-huffman-tree pairs)
  (define (successive-merge leaves)
    (if (null? (cdr leaves))
        (car leaves)
        (let ((merged (make-code-tree (car leaves) (cadr leaves)))
              (left (cddr leaves)))
          (successive-merge (adjoin-set merged left)))))
  (successive-merge (make-leaf-set pairs)))

;test
(define sample-symbol-pairs '((A 4) (B 2) (C 1) (D 1) (E 5) (F 1) (G 3) (H 1)))
(generate-huffman-tree sample-symbol-pairs)
;; (((leaf A 4)
;;   ((leaf B 2) ((leaf H 1) (leaf F 1) (H F) 2) (B H F) 4)
;;   (A B H F)
;;   8)
;;  ((leaf E 5)
;;   (((leaf D 1) (leaf C 1) (D C) 2) (leaf G 3) (D C G) 5)
;;   (E D C G)
;;   10)
;;  (A B H F E D C G)
;;  18)

;;; Exercise 2.70

(define fifties-rock-huffman-tree
  (generate-huffman-tree '((a 2) (boom 1)(get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1))))

(encode '(get a job
              sha na na na na na na na na
              get a job
              sha na na na na na na na na
              wah yip yip yip yip yip yip yip yip yip
              sha boom) fifties-rock-huffman-tree)
;(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)
(decode '(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1) fifties-rock-huffman-tree)

;;; Exercise 2.71

;n=5
;; (A 1) (B 2) (C 4) (D 8) (E 16)
;; ((A 1) (B 2) {A B} 3) (C 4) (D 8) (E 16)
;; (((A 1) (B 2) {A B} 3) (C 4) {A B C} 7) (D 8) (E 16)
;; ((((A 1) (B 2) {A B} 3) (C 4) {A B C} 7) (D 8) {A B C D} 15) (E 16)
;; (((((A 1) (B 2) {A B} 3) (C 4) {A B C} 7) (D 8) {A B C D} 15) (E 16) {A B C D E} 31)

(((((A 1)                               ;00001 WRONG!
    (B 2)                               ;0001
    {A B} 3)
   (C 4)                                ;001
   {A B C} 7)
  (D 8)                                 ;01
  {A B C D} 15)
 (E 16)                                 ;1
 {A B C D E} 31)

;the most frequent symbol is encoded by one bit. The least frequent by
;n bits.
;WRONE! A is coded with 0000! Which is n-1 for the two least
;frequent symbols.

;;; Exercise 2.72

;For both most common and least common symbols, this implementation calls in-branch on the entire tree before starting the encoding in order to clear the possibility of error. This call has an average of O(n/2), but then in-branch only has to be called for one branch in each recursion (since the symbol must be on either branches)

;For the most common symbol, build-code is called only once after the initial call, since the second call gives the leaf. append is called once on an empty list. Thus the order of growth is O(n/2+n/2) = O(n).

;For the least common symbol, build-code is called n-1 times and append is called n-1 times with a growing list. Thus O(n/2+(n-1)*n/2+(n-1)/2)
;= O((n^2+n-1)/2) ~ O(n^2)
