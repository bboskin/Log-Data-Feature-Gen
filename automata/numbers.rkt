#lang racket

(require "basics.rkt" csv-writing)

(define MAX 500)


(define (k-factors x)
  (length
   (to-set
    (filter
     (λ (i) (and (< i x)
                 (> i 1)
                 (zero? (remainder x i))))
     (build-list x (λ (x) x))))))

(define (k-int-sqr-roots x)
  (length
   (to-set
    (filter
     (λ (i) (= (* i i) x))
     (build-list x (λ (x) x))))))

(define (k-int-cube-roots x)
  (length
   (to-set
    (filter
     (λ (i) (= (* i i i) x))
     (build-list x (λ (x) x))))))

(define ((rem k) x) (zero? (remainder x k)))

(define (kth-fib x)
  (if (zero? x)
      0
      (let loop ((k0 0)
                 (k1 1)
                 (i 1))
        (cond
          [(> k1 x) -1]
          [(= (+ k0 k1) x) i]
          [else (loop k1 (+ k0 k1) (add1 i))]))))

(define (collatz x)
  (cond
    [(zero? x) '(1)]
    [(= x 1) '(1)]
    [(even? x) (cons x (collatz (/ x 2)))]
    [else (cons x (collatz (+ 1 (* 3 x))))]))

(define (k-collatz-steps x) (length (collatz x)))


(define COLUMN-FNS
  `(,(λ (x) x)
    ,k-factors
    . ,(build-list MAX (λ (x) (rem (add1 x))))))

(display-table
 (cons
  (cons 'name (build-list (sub1 MAX) (λ (x) (string->symbol (string-append "F" (number->string x))))))
  (build-list MAX (λ (x) (map (λ (f) (f (add1 x))) COLUMN-FNS)))))

