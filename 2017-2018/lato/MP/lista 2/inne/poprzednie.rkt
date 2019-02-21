#lang racket

(define (identity x)
  x)

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

;; ćwiczenie 2
(define (compose f g)
  (lambda (x) (f (g x))))

;; ćwiczenie 3
(define (repeated p n)
  (cond
    [(= n 0) p]
    [else (compose p (repeated p (- n 1)))]))

;; ćwiczenie 4
(define (product-rec term next s e)
  (cond
    [(> s e) 1]
    [else (* (term s) (product-rec term next (next s) e))]))

(define (product-iter term next accumulator s e)
  (cond
    [(> s e) accumulator]
    [else (product-iter term next (* accumulator (term s)) (next s) e)]))

(define (pi-using-product a)
  (* (/ (* 4 2) (* 3 3)) (/
                          (/ (product-iter square (lambda (x) (+ x 2)) 1.0 4 a) a)
                          (product-iter square (lambda (x) (+ x 2)) 1.0 5 (- a 1)))))

;; ćwiczenie 5
(define (accumulate-rec combiner null-value term a next b)
  (cond
    [(> a b) null-value]
    [else (combiner (term a) (accumulate-rec combiner null-value term (next a) next b))]))

(define (accumulate-iter combiner term a next b accumulator)
  (cond
    [(> a b) accumulator]
    [else (accumulate-iter combiner term (combiner accumulator (term a)) (next a) b)]))

;; ćwiczenie 6
(define (one i)
  1.0)

(define (cont-frac-rec num den k)
  {cond
    [(= k 0) 0]
    [else (/ (num k) (+ (den k) (cont-frac-rec num den (- k 1))))]
  })

;; ćwiczenie 9

(define (build n d b)
  (/ n (+ d b)))

(define (repeated-build k n d b)
  {cond
    [(= k 1) (build n d b)]
    [else (repeated-build (- k 1) n d (build n d b))]
  })
  