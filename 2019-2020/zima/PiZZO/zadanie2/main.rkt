#! /usr/bin/racket
#lang racket

(require json)
(require racket/set)

;;; converts the JSON problem instance into a graph
(define (to-graph problem)
    ;;; new graph
    (define empty-graph 
        (make-hasheq '()))
    ;;; destructively adds edges <v1, v2> and <v2,v1> to the graph
    (define (add-edge! graph v1 v2)
        ;;; get the set of neighbours of the given vertex. Create the set if
        ;;; needed
        (define (get-neighbours vertex)
            (if (hash-has-key? graph vertex)
                (hash-ref graph vertex)
                (begin
                    (hash-set! graph vertex (mutable-seteq))
                    (hash-ref graph vertex))))
        (let ([v1-neighbours (get-neighbours v1)]
              [v2-neighbours (get-neighbours v2)])
                (set-add! v1-neighbours v2)
                (set-add! v2-neighbours v1)))
    ;;; converts list of conflicts into a graph
    (define (conflicts-to-graph conflicts graph)
        (for ([conflict conflicts])
            (let ([from (hash-ref conflict 'zrzeda)]
                    [to (hash-ref conflict 'nielubiany)])
                        (add-edge! graph from to)))
        graph)
    (conflicts-to-graph (hash-ref problem 'konflikty) empty-graph))

;;; reduces the size of the graph by removing uneccessary vertices
(define (reduce-graph! graph)
    ;;; destructively removes vertices of degree lesser than 4 and 
    ;;; returns the number of removed vertices
    (define (reduce-once! graph)
        (let ([count 0])
            (hash-for-each graph
                (lambda (vertex neighbours)
                    (when (< (set-count neighbours) 4)
                        (hash-remove! graph vertex)
                        (set! count (+ 1 count)))))
            count))
    (if (= (reduce-once! graph) 0)
        graph
        (reduce-graph! graph)))

;;; converts the graph into SAT problem instance
(define (to-sat graph)
    ;TODO: implement
    graph)

;;; prints the SAT instance in the correct format
(define (print-sat sat)
    ;TODO: implement
    (fprintf (current-output-port) "~a" sat))

(module+ main
    (let* ([problem (read-json)]
           [graph (to-graph problem)]
           [reduced-graph (reduce-graph! graph)]
           [sat (to-sat reduced-graph)])
        (print-sat sat)))
