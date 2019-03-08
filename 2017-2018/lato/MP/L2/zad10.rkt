#lang racket

;; helper procedures
(define (++ x)
  {+ x 1})
(define (-- x)
  {- x 1})
(define (square x)
  {* x x})

;; procedure that calculates the approximated value of continued fraction
;; N - procedure that calculates the i-th numerator
;; D - procedure that calculates the i-th denominator
(define (cont-frac-approx N D)
  ;; procedure that calculates An or Bn iteratively
  (define (AB i end prev next)
    {cond
      [(= end -1) prev]
      [(= end 0)  next]
      [(= i end) next]
      [else (AB (++ i) end next (+ (* (D i) next) (* (N i) prev)))]})
  ;; procedure that checks whether the approximation is good enough
  (define (good-enough? prev new)
    (define precision 0.00001)
    (define (abs x)
      {if (< x 0) (- x) x})
    {< (abs (- prev new)) precision})
  ;; procedure that calculates the current approximation of the function (fk = Ak/Bk)
  (define (f k)
    {/ (AB 1 k 1 0) (AB 1 k 0 1)})
  ;; procedure that iterates through the values of k to find the best approximation
  (define (iter prev new k)
    {cond
      [(good-enough? prev new) new]
      [else (iter new (f (++ k)) (++ k))]})
  {iter 0.0 1.0 4})

;; TESTS:

;; golden ratio:
(display "golden ratio: ")
(/ 1.0 (cont-frac-approx (lambda (x) 1) (lambda (x) 1)))

;; pi:
(display "pi: ")
(+ 3.0 (cont-frac-approx (lambda (i) {square (+ 1 (* (-- i) 2))}) (lambda (x) 6)))
