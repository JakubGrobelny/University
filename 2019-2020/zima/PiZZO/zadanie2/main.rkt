#! /usr/bin/racket
#lang racket

(require json)
(require racket/set)

;;; converts the JSON problem instance into a graph
(define (instance->graph problem)
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

(struct sat-instance (var-count clauses))

;;; converts the graph into SAT problem instance
(define (graph->sat-instance graph)
    ;;; get variable name for a given student and group
    (define (var-name student group)
        (+ student group))
    ;;; build a dictionary of fresh names for vertices
    (define (rename-vertices)
        (let ([names (make-hasheq)]
              [count 1])
            (hash-for-each graph
                (lambda (vertex _)
                    (hash-set! names vertex count)
                    ; each student has 4 variables
                    (set! count (+ 4 count))))
        names))
    ;;; build a set of pairs that are in conflicts without repretitions
    (define (create-unique-pairs names)
        (define (make-pair a b)
            (if (> a b)
                (cons b a)
                (cons a b)))
        (let ([pairs (mutable-seteq)])
            (hash-for-each graph
                (lambda (vertex neighbours)
                    (set-for-each neighbours
                        (lambda (neighbour)
                            (let* ([v0 (hash-ref names vertex)]
                                   [v1 (hash-ref names neighbour)]
                                   [pair (make-pair v0 v1)])
                                (set-add! pairs pair))))))
            pairs))
    ;;; builds a list of clauses that encode the requirement of
    ;;; being in exactly one group
    (define (build-initial-clauses name-count)
        ;;; clause encoding being in at least one group
        (define (build-any-group-clause var)
            (list (var-name var 0)
                  (var-name var 1)
                  (var-name var 2)
                  (var-name var 3)))
        ;;; clause encoding being in at most one group
        (define (build-one-group-clauses var)
            (let ([g1 (- (var-name var 0))]
                  [g2 (- (var-name var 1))]
                  [g3 (- (var-name var 2))]
                  [g4 (- (var-name var 3))])
                (list (list g1 g2)
                      (list g1 g3)
                      (list g1 g4)
                      (list g2 g3)
                      (list g2 g4)
                      (list g3 g4))))
        (if (= name-count 0)
            '()
            (let* ([name (+ 1 (* 4 (- name-count 1)))]
                   [any-group (build-any-group-clause name)]
                   [one-group (build-one-group-clauses name)])
                (append (cons any-group one-group)
                        (build-initial-clauses (- name-count 1))))))
    ;;; makes a clause for every conflict
    (define (build-clauses pairs initial-clauses)
        ;;; converts a single conflict into 4 clauses
        (define (pair-to-clauses pair)
            ;;; converts a conflict and a group into a corresponding clause
            (define (group-pair-to-clause group)
                (let ([v0 (var-name (car pair) group)]
                      [v1 (var-name (cdr pair) group)])
                    (list (- v0) (- v1))))
            (map group-pair-to-clause '(0 1 2 3)))
        (if (null? pairs)
            initial-clauses
            (append 
                (pair-to-clauses (car pairs))
                (build-clauses (cdr pairs) initial-clauses))))
    (let* ([names (rename-vertices)]
           [pairs (set->list (create-unique-pairs names))]
           [initial-clauses (build-initial-clauses (hash-count names))])
        (sat-instance 
            (* 4 (hash-count names)) 
            (remove-duplicates (build-clauses pairs initial-clauses)))))

;;; prints the SAT instance in the correct format
(define (print-sat sat)
    ;;; print CNF clauses in separate lines
    (define (print-clauses clauses)
        ;;; print a single clause
        (define (print-clause clause)
            (map (lambda (var) (fprintf (current-output-port) "~a " var)) 
                 clause))
        (unless (null? clauses)
            (print-clause (car clauses))
            (fprintf (current-output-port) "0\n")
            (print-clauses (cdr clauses))))
    (let* ([var-count (sat-instance-var-count sat)]
           [clauses (sat-instance-clauses sat)]
           [clause-count (length clauses)])
        (fprintf (current-output-port) "p cnf ~a ~a\n" var-count clause-count)
        (print-clauses clauses)))

(module+ main
    (let* ([problem (read-json)]
           [graph (instance->graph problem)]
           [reduced-graph (reduce-graph! graph)]
           [sat (graph->sat-instance reduced-graph)])
        (print-sat sat)))
