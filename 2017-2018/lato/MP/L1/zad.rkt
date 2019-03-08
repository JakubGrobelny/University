#lang racket

;; program based on the square root program from the "Structure and
;; Interpretation of Computer Programs" by Harold Abelson,
;; Gerald Jay Sussman and Julie Sussman

;; procedure that finds the square root of the given number 'x'
(define (cube-root x)
  ;; procedure that improves the current approximation
  (define (improve approx)
    ;; procedure that raises 'approx' to the power of 2
    (define (square)
      (* approx approx))
    (/ (+ (/ x (square)) (* 2 approx)) 3))
  ;; recursive procedure that finds better approximation in every iteration
  (define (iter approx)
    ;; procedure that raises given 'a' to the power of 3
    (define (cube a)
      (* a a a))
    ;; procedure that finds the relative error between 'x' and approximation
    (define (relative-error)
      (/ (abs (- x (cube approx))) (+ 1 (abs x))))
    ;; procedure that checks whether the current approximation is accurate enough
    (define (good-enough?)
      (< (relative-error) 0.0001)) ;; 0.01% accuracy (in theory)
    (cond
      [(good-enough?) approx]
      [else (iter (improve approx))]))
  (iter 1.0))

;; tests
(cube-root  0)
(cube-root  0.1)
(cube-root -1)
(cube-root  125)
(cube-root -27)
(cube-root -0.1)
(cube-root  2197)
(cube-root 1000)
(cube-root (- (cube-root 1000000000)))
