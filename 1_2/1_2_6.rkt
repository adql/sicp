#lang sicp

;;; -----------------------------------------------
;;; 1.2  Procedures and the Processes They Generate
;;; -----------------------------------------------

;;; 1.2.6 Example: Testing for Primality

;;; Streightforward way

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;;; The Fermat test

(define (square x) (* x x))             ;hasn't been defined so far...

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;;; Exercise 1.22

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n)
  (cond ((even? n) (search-for-primes (+ n 1)))
        ((prime? n) (timed-prime-test n))
        (else (search-for-primes (+ n 2)))))

;; The original ranges suggested in the book are too low for today's
;; fast computers
(search-for-primes 10000000000)         ;4025
(search-for-primes 10000000020)         ;5980
(search-for-primes 10000000034)         ;4070

(search-for-primes 100000000000)        ;13545
(search-for-primes 100000000004)        ;14914
(search-for-primes 100000000020)        ;14516

(search-for-primes 1000000000000)       ;40352
(search-for-primes 1000000000040)       ;39411
(search-for-primes 1000000000062)       ;40618

(search-for-primes 10000000000000)      ;120015
(search-for-primes 10000000000038)      ;119228
(search-for-primes 10000000000051)      ;119650

;; Exercise 1.23

(define (next n)
  (if (= n 2) 3 (+ n 2)))

(define (smallest-divisor2 n)
  (find-divisor2 n 2))

(define (find-divisor2 n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor2 n (next test-divisor)))))

(define (prime?2 n)
  (= n (smallest-divisor2 n)))

(define (timed-prime-test2 n)
  (newline)
  (display n)
  (start-prime-test2 n (runtime)))

(define (start-prime-test2 n start-time)
  (if (prime?2 n)
      (report-prime (- (runtime) start-time))))

;; Redifining and reusing just for convenience
(define (search-for-primes2 n)
  (cond ((even? n) (search-for-primes2 (+ n 1)))
        ((prime?2 n) (timed-prime-test2 n))
        (else (search-for-primes2 (+ n 2)))))

(search-for-primes2 10000000000)        ;4755
(search-for-primes2 10000000020)        ;7133
(search-for-primes2 10000000034)        ;5175

(search-for-primes2 100000000000)       ;11315
(search-for-primes2 100000000004)       ;11554
(search-for-primes2 100000000020)       ;8000

(search-for-primes2 1000000000000)      ;25965
(search-for-primes2 1000000000040)      ;27146
(search-for-primes2 1000000000062)      ;27117

(search-for-primes2 10000000000000)     ;76794
(search-for-primes2 10000000000038)     ;73927
(search-for-primes2 10000000000051)     ;77505

;; It's significanlty faster but less than twice as fast (more around
;; 1.5 as fast from fast impression). Possibly testing whether
;; test-divisor is equal to or greater than 2 accumulates to more
;; computation time (it needs to be tested test-divisor^-2 times)

;; Exercise 1.24

;; fermat-test has to be modified since random can't accept very large
;; numbers
(define (fast-prime?3 n times)
  (cond ((= times 0) true)
        ((fermat-test3 n) (fast-prime?3 n (- times 1)))
        (else false)))

(define (fermat-test3 n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (if (< n 4294967088)
                           (- n 1) 4294967087)))))

(define (timed-prime-test3 n)
  (newline)
  (display n)
  (start-prime-test3 n (runtime)))

(define (start-prime-test3 n start-time)
  (if (fast-prime?3 n 5)
      (report-prime (- (runtime) start-time))))

;; Redifining and reusing just for convenience
(define (search-for-primes3 n)
  (cond ((even? n) (search-for-primes3 (+ n 1)))
        ((fast-prime?3 n 5) (timed-prime-test3 n))
        (else (search-for-primes3 (+ n 2)))))

(search-for-primes3 10000000000)        ;375
(search-for-primes3 10000000020)        ;176
(search-for-primes3 10000000034)        ;93

(search-for-primes3 100000000000)       ;196
(search-for-primes3 100000000004)       ;129
(search-for-primes3 100000000020)       ;216

(search-for-primes3 1000000000000)      ;270
(search-for-primes3 1000000000040)      ;239
(search-for-primes3 1000000000062)      ;299

(search-for-primes3 10000000000000)     ;147
(search-for-primes3 10000000000038)     ;150
(search-for-primes3 10000000000051)     ;233

;; Seems like there's no change at all!

(search-for-primes3 1000000000000000000) ;215
(search-for-primes3 10000000000000000000) ;705
(search-for-primes3 100000000000000000000) ;707
(search-for-primes3 1000000000000000000000) ;333
(search-for-primes3 10000000000000000000000) ;345

(search-for-primes3 100000000000000000000000000000000) ;1225
(search-for-primes3 1000000000000000000000000000000000000000000) ;1576

;; e100
;(search-for-primes3 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000) ;4679

;; e1000
;(search-for-primes3 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000) ;227624
;; roughly 10 seconds

;; Exercise 1.27

(define (demonstrate-carmichael n)
  (define (test-iter a)
    (if (= (expmod a n n) a)
        (if (= a (- n 1))
            #t
            (test-iter (+ a 1)))
        #f))
  (test-iter 2))

;; testing the function
(demonstrate-carmichael 23)             ;#t
(demonstrate-carmichael 40)             ;#f

;; demonstrating
(demonstrate-carmichael 561)            ;#t
(demonstrate-carmichael 1105)           ;#t
(demonstrate-carmichael 1729)           ;#t
(demonstrate-carmichael 2465)           ;#t
(demonstrate-carmichael 2821)           ;#t
(demonstrate-carmichael 6601)           ;#t

;; Exercise 1.28

(define (expmod2 base exp m)
  (define (non-trivial-sqrt? b sqr_remainder)
    (and (not (= b 1))
         (< b (- m 1))
         (= sqr_remainder 1)))
  (cond ((= exp 0) 1)
        ((even? exp)
         (let* ((b (expmod2 base (/ exp 2) m))
                (sqr_remainder (remainder (square b) m)))
           (if (non-trivial-sqrt? b sqr_remainder) 0 sqr_remainder)))
        (else
         (remainder (* base (expmod2 base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod2 a (- n 1) n) 1))
  (try-it (+ 1 (random (if (< n 4294967088)
                           (- n 1) 4294967087)))))

;since nonprime odds may pass miller-rabin-test for some a's, it needs
;to be repeated
(define (fast-prime?4 n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime?4 n (- times 1)))
        (else false)))

;testing known primes and non-primes
(fast-prime?4 23 5)
(fast-prime?4 25 5)
(fast-prime?4 100000000057 5)
(fast-prime?4 100000000058 5)

;; demonstrating
(fast-prime?4 561 5)                    ;f
(fast-prime?4 1105 5)                   ;f
(fast-prime?4 1729 5)                   ;f
(fast-prime?4 2465 5)                   ;f
(fast-prime?4 2821 5)                   ;f
(fast-prime?4 6601 5)                   ;f
