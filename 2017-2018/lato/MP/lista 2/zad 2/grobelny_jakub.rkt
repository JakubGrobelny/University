#lang racket

;; program is using procedures from
;; https://skos.ii.uni.wroc.pl/pluginfile.php/14639/mod_resource/content/1/wyklad-2.rkt

;; helper functions
(define (-- x)
  {- x 1})
(define (++ x)
  {+ x 1})

;; function that performs average damping damp-amount times
;; used to find the amount of average dampings that is required
;; to get the correct result (proper nth-root function is in the testing section below)
(define (nth-root-experimental n number damp-amount)
  ;; function that composes function p n times
  (define (repeated p n)
    (define (compose f g)
      {lambda (x) (f (g x))})
    {cond
      [(= n 0) p]
      [else (compose p (repeated p (-- n)))]})
  ;; function that finds the fixed point of the function f
  (define (fixed-point f x0)
    (define (close-enough? x y)
      (define (dist x y)
        (define (abs x)
          {if (< x 0) (- x) x})
        {abs (- x y)})
      {let ([accuracy 0.00001])
        (< (dist x y) accuracy)})
    {let ((x1 (f x0)))
      (cond
        [(close-enough? x0 x1) x0]
        [else (fixed-point f x1)])})
  ;; average damping
  (define (average-damp f)
    {lambda (x) (/ (+ x (f x)) 2)})
  {if (= number 0)
      0.0
      (fixed-point ((repeated average-damp damp-amount) (lambda (y) (/ number (expt y damp-amount)))) 1.0)})


;; tests to find the correct damp-amount
(display "Finding correct damp-amount: \n")

(display "\t4th root of 81:\n")
(display "\t  damp-amount = 2 -> ")
(nth-root-experimental 4 81 2)
(display "\t  damp-amount = 3 -> ")
(nth-root-experimental 4 81 3) ;; correct (4th root -> damp-amount = 4)
(display "\t  damp-amount = 4 -> ")
(nth-root-experimental 4 81 4)

(display "\t7th root of 128:\n")
(display "\t  damp-amount = 5 -> ")
(nth-root-experimental 7 128 5)
(display "\t  damp-amount = 6 -> ")
(nth-root-experimental 7 128 6) ;; correct (7th root -> damp-amount = 6)
(display "\t  damp-amount = 7 -> ")
(nth-root-experimental 7 128 7)

;; Based on above observations we can deduce, that you need to do average damping n-1 times

;; proper nth-root function that uses average damping (n - 1) times
(define (nth-root n number)
  {nth-root-experimental n number (-- n)})

(display "\nTests of the actual function: \n")
(display "\tSquare root of 4: ")
(nth-root 2 4)
(display "\tCubic root of -27: ")
(nth-root 3 -27)
(display "\tSquare root of 36: ")
(nth-root 2 36)
(display "\t5th root of 3125: ")
(nth-root 5 3125)
(display "\tSquare root of 2: ")
(nth-root 2 2)
(display "\t100th root of 0: ")
(nth-root 100 0)