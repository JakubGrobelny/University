#lang racket

; ćwiczenie 1

(define (make-rat num den)
  (cons num (cons den null)))

(define (rat-num x)
  (car x))

(define (rat-den x)
  (car (cdr x)))

(define (rat? x)
  (if (not (pair? x))
      false
      (if (not (pair? (cdr x)))
          false
          (if (= (car (cdr x)) 0) ; prawie (jak jest faktycznie para to się sypie)
              false
              (if (null? (cdr (cdr x)))
                  true
                  false)))))      

;; ćwiczenie 2

(define (make-point x y)
  (if (and (number? x) (number? y))
      (cons x y)
      null))

(define (point? p)
  (if (pair? p)
      (if (and (number? (car p)) (number? (cdr p)))
          true
          false)
      false))

(define (point-x p)
  (if (point? p)
      (car p)
      null))

(define (point-y p)
  (if (point? p)
      (cdr p)
      null))

(define (make-vect p1 p2)
  (if (and (point? p1) (point? p2))
      (cons p1 p2)
      null))

(define (vect? v)
  (if (pair? v)
      (if (and (point? (car v)) (point? (cdr v)))
          true
          false)
      false))

(define (vect-begin v)
  (if (vect? v)
      (car v)
      null))

(define (vect-end v)
  (if (vect? v)
      (cdr v)
      null))

( define ( display-point p )
   ( display "(")
   ( display ( point-x p ) )
   ( display ", ")
   ( display ( point-y p ) )
   ( display ")") )

( define ( display-vect v )
   ( display "[")
   ( display-point ( vect-begin v ) )
   ( display ", ")
   ( display-point ( vect-end v ) )
   ( display "]") )

(define (square x)
  (* x x))

(define (abs x)
  (if (> 0 x)
      (- x)
      x))

(define (vect-length v)
  (if (vect? v)
      (expt (+ (square (abs (- (point-x (vect-begin v)) (point-x (vect-end v))))) (square (abs (- (point-y (vect-begin v)) (point-y (vect-end v)))))) 0.5)
      0))

(define (vect-scale v k)
  (if (vect? v)
      (make-vect (vect-begin v)
                 (make-point (* k (point-x (vect-end v)))
                             (* k (point-y (vect-end v)))))
      null))

(define (vect-translate v p)
  (if (vect? v)
      (let ([x-offset (- (point-x (vect-begin v)) (point-x p))]
            [y-offset (- (point-y (vect-begin v)) (point-y p))])
        (make-vect p (make-point
                      (- (point-x (vect-end v)) x-offset)
                      (- (point-y (vect-end v)) y-offset))))
      null))

(define p1 (make-point 0 0))
(define p2 (make-point 1 1))
(define p3 (make-point -1 -1))
(define v (make-vect p1 p2))

