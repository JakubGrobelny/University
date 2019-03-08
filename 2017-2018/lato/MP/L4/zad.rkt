#lang racket

(define (inc n)
  (+ n 1))

;;; ordered elements
(define (make-elem pri val)
  (cons pri val))

(define (elem-priority x)
  (car x))

(define (elem-val x)
  (cdr x))

;;; leftist heaps (after Okasaki)

;; data representation
(define leaf 'leaf)

(define (leaf? h) (eq? 'leaf h))

(define (hnode? h)
  (and (list? h)
       (= 5 (length h))
       (eq? (car h) 'hnode)
       (natural? (caddr h))))

(define (make-node elem heap-a heap-b)
  (cond
    ; if heap-a's rank was bigger then it must become the left subtree
    [(> (rank heap-a) (rank heap-b)) (list 'hnode elem (inc (rank heap-b)) heap-a heap-b)]
    [else (list 'hnode elem (inc (rank heap-a)) heap-b heap-a)]))

(define (node-elem h)
  (second h))

(define (node-left h)
  (fourth h))

(define (node-right h)
  (fifth h))

(define (hord? p h)
  (or (leaf? h)
      (<= p (elem-priority (node-elem h)))))

(define (heap? h)
  (or (leaf? h)
      (and (hnode? h)
           (heap? (node-left h))
           (heap? (node-right h))
           (<= (rank (node-right h))
               (rank (node-left h)))
           (= (rank h) (inc (rank (node-right h))))
           (hord? (elem-priority (node-elem h))
                  (node-left h))
           (hord? (elem-priority (node-elem h))
                  (node-right h)))))

(define (rank h)
  (if (leaf? h)
      0
      (third h)))

;; operations

(define empty-heap leaf)

(define (heap-empty? h)
  (leaf? h))

(define (heap-insert elt heap)
  (heap-merge heap (make-node elt leaf leaf)))

(define (heap-min heap)
  (node-elem heap))

(define (heap-pop heap)
  (heap-merge (node-left heap) (node-right heap)))

(define (heap-merge h1 h2)
  (cond
   [(leaf? h1) h2]
   [(leaf? h2) h1]
   ; finding which heap has smaller priority at it's root
   [else (let ([e1 (elem-priority (heap-min h1))]
               [e2 (elem-priority (heap-min h2))])
           (let ([smaller (if (> e1 e2) h2 h1)]
                 [bigger (if (> e1 e2) h1 h2)])
             ; e - new root
             ; hr - right subtree of the heap that contained e
             ; hl - left subtree ...
             (let ([e (heap-min smaller)]
                   [hr (node-right smaller)]
                   [hl (node-left smaller)])
               ; creating a new heap with root e, hl as left subtree
               ; and subtree created by merging hr and the heap that
               ; had bigger root
               (make-node e hl (heap-merge hr bigger)))))]))

;;; heapsort. sorts a list of numbers.
(define (heapsort xs)
  (define (popAll h)
    (if (heap-empty? h)
        null
        (cons (elem-val (heap-min h)) (popAll (heap-pop h)))))
  (let ((h (foldl (lambda (x h)
                    (heap-insert (make-elem x x) h))
            empty-heap xs)))
    (popAll h)))

;;; check that a list is sorted (useful for longish lists)
(define (sorted? xs)
  (cond [(null? xs)              true]
        [(null? (cdr xs))        true]
        [(<= (car xs) (cadr xs)) (sorted? (cdr xs))]
        [else                    false]))

;;; generate a list of random numbers of a given length
(define (randlist len max)
  (define (aux len lst)
    (if (= len 0)
        lst
        (aux (- len 1) (cons (random max) lst))))
  (aux len null))

;;; TESTS ;;;

; small lists
(display "Small tests: \n")
(heapsort (randlist 10 100))
(heapsort (randlist 10 100))
(heapsort (randlist 10 100))
(heapsort (randlist 10 100))
(heapsort (randlist 10 100))
(heapsort (randlist 10 100))
;; bigger lists
(display "Bigger tests (testing heapsort with 'sorted?'):\n")
(sorted? (heapsort (randlist 100 1000)))
(sorted? (heapsort (randlist 100 1000)))
(sorted? (heapsort (randlist 100 1000)))
(sorted? (heapsort (randlist 100 1000)))
(sorted? (heapsort (randlist 100 1000)))
(sorted? (heapsort (randlist 100 1000)))
(sorted? (heapsort (randlist 1000 10000)))
(sorted? (heapsort (randlist 1000 10000)))
(sorted? (heapsort (randlist 1000 10000)))
(sorted? (heapsort (randlist 1000 10000)))
(sorted? (heapsort (randlist 1000 10000)))
(sorted? (heapsort (randlist 10000 100000)))
(sorted? (heapsort (randlist 10000 100000)))
(sorted? (heapsort (randlist 10000 100000)))
(sorted? (heapsort (randlist 10000 100000)))
(sorted? (heapsort (randlist 10000 100042)))