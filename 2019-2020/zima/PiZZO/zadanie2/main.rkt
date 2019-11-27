#! /usr/bin/racket
#lang racket

(require json)

(define (to-graph problem)
    ; TODO: implement
    problem)

(define (reduce-graph graph)
    ; TODO: implement
    graph)

(define (to-sat graph)
    ;TODO: implement
    graph)

(define (print-sat sat)
    ;TODO: implement
    (fprintf (current-error-port) "~a" sat))

(module+ main
    (let* ([problem (read-json)]
           [graph (to-graph problem)]
           [reduced-graph (reduce-graph graph)]
           [sat (to-sat reduced-graph)])
        (print-sat sat)))
