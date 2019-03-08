#lang racket

;; sygnatura: grafy
(define-signature graph^
  ((contracted
    [graph        (-> list? (listof edge?) graph?)]
    [graph?       (-> any/c boolean?)]
    [graph-nodes  (-> graph? list?)]
    [graph-edges  (-> graph? (listof edge?))]
    [edge         (-> any/c any/c edge?)]
    [edge?        (-> any/c boolean?)]
    [edge-start   (-> edge? any/c)]
    [edge-end     (-> edge? any/c)]
    [has-node?    (-> graph? any/c boolean?)]
    [outnodes     (-> graph? any/c list?)]
    [remove-node  (-> graph? any/c graph?)]
    )))

;; prosta implementacja grafów
(define-unit simple-graph@
  (import)
  (export graph^)

  (define (graph? g)
    (and (list? g)
         (eq? (length g) 3)
         (eq? (car g) 'graph)))

  (define (edge? e)
    (and (list? e)
         (eq? (length e) 3)
         (eq? (car e) 'edge)))

  (define (graph-nodes g) (cadr g))

  (define (graph-edges g) (caddr g))

  (define (graph n e) (list 'graph n e))

  (define (edge n1 n2) (list 'edge n1 n2))

  (define (edge-start e) (cadr e))

  (define (edge-end e) (caddr e))

  (define (has-node? g n) (not (not (member n (graph-nodes g)))))
  
  (define (outnodes g n)
    (filter-map
     (lambda (e)
       (and (eq? (edge-start e) n)
            (edge-end e)))
     (graph-edges g)))

  (define (remove-node g n)
    (graph
     (remove n (graph-nodes g))
     (filter
      (lambda (e)
        (not (eq? (edge-start e) n)))
      (graph-edges g)))))

;; sygnatura dla struktury danych
(define-signature bag^
  ((contracted
    [bag?       (-> any/c boolean?)]
    [bag-empty? (-> bag? boolean?)]
    [empty-bag  (and/c bag? bag-empty?)]
    [bag-insert (-> bag? any/c (and/c bag? (not/c bag-empty?)))]
    [bag-peek   (-> (and/c bag? (not/c bag-empty?)) any/c)]
    [bag-remove (-> (and/c bag? (not/c bag-empty?)) bag?)])))

;; struktura danych - stos
(define-unit bag-stack@
  (import)
  (export bag^)
  
  (define (stack-elems s)
    (cdr s))
  
  (define (bag? b)
    (and (pair? b)
         (eq? (car b) 'stack)
         (list? (stack-elems b))))

  (define (bag-empty? b)
    (and (bag? b)
         (null? (stack-elems b))))

  (define empty-bag
    (cons 'stack '()))

  (define (bag-insert b e)
    (cons 'stack (cons e (stack-elems b))))

  (define (bag-peek b)
    (car (stack-elems b)))

  (define (bag-remove b)
    (cons 'stack (cdr (stack-elems b)))))

;; struktura danych - kolejka FIFO
;; do zaimplementowania przez studentów
(define-unit bag-fifo@
  (import)
  (export bag^)

  (define (fifo-in f)
    (second f))
  
  (define (fifo-out f)
    (third f))

  (define(fifo-swap f)
    (list 'fifo '() (reverse (fifo-in f))))
  
  (define (bag? b)
    (and (list? b)
         (= 3 (length b))
         (eq? (car b) 'fifo)
         (and (list? (fifo-in  b))
              (list? (fifo-out b)))))

  (define (bag-empty? b)
    (and (bag? b)
         (null? (fifo-in b))
         (null? (fifo-out b))))

  (define empty-bag
    (list 'fifo '() '()))

  (define (bag-insert b e)
    (list 'fifo (cons e (fifo-in b))
                (fifo-out b)))

  (define (bag-peek b)
    (if (null? (fifo-out b))
        (car (fifo-out (fifo-swap b)))
        (car (fifo-out b))))

  (define (bag-remove b)
    (let ([fifo (if (null? (fifo-out b))
                    (fifo-swap b)
                    b)])
      (list 'fifo (fifo-in fifo)
                  (cdr (fifo-out fifo))))))

;; sygnatura dla przeszukiwania grafu
(define-signature graph-search^
  (search))

;; implementacja przeszukiwania grafu
;; uzależniona od implementacji grafu i struktury danych
(define-unit/contract graph-search@
  (import bag^ graph^)
  (export (graph-search^
           [search (-> graph? any/c (listof any/c))]))
  (define (search g n)
    (define (it g b l)
      (cond
        [(bag-empty? b) (reverse l)]
        [(has-node? g (bag-peek b))
         (it (remove-node g (bag-peek b))
             (foldl
              (lambda (n1 b1) (bag-insert b1 n1))
              (bag-remove b)
              (outnodes g (bag-peek b)))
             (cons (bag-peek b) l))]
        [else (it g (bag-remove b) l)]))
    (it g (bag-insert empty-bag n) '()))
  )

;; otwarcie komponentu grafu
(define-values/invoke-unit/infer simple-graph@)

;; graf testowy
(define test-graph
  (graph
   (list 1 2 3 4)
   (list (edge 1 3)
         (edge 1 2)
         (edge 2 4))))
;; TODO: napisz inne testowe grafy!

(define my-test-graph1
  (graph
   (list 'a 'b 'c 'd 'e 'f)
   (list (edge 'a 'd) (edge 'a 'c)
         (edge 'c 'b) (edge 'b 'f)
         (edge 'c 'f) (edge 'f 'e))))

(define my-test-graph2
  (graph
   (list 0 1 2 3 4 5 6)
   (list (edge 1 2) (edge 1 3) (edge 2 4)
         (edge 4 0) (edge 2 5) (edge 5 0)
         (edge 5 6) (edge 6 0) (edge 3 6))))

(define my-test-graph3
  (graph
   (list 0 1 2 3 4 5 6 7 8 9 10 11 12)
   (list (edge 0 1) (edge 1 2) (edge 2 3) (edge 3 4)
         (edge 4 5) (edge 5 6) (edge 6 7) (edge 7 8)
         (edge 8 9) (edge 9 10) (edge 10 11) (edge 11 12))))

(define my-test-graph4
  (graph
   (list 0 1 2 3 4 5)
   (list (edge 0 1) (edge 1 2) (edge 2 3)
         (edge 3 4) (edge 4 0) (edge 4 5))))

(define my-test-graph5
  (graph
   (list 0 1 2 3 4 5)
   (list (edge 0 1) (edge 0 2)
         (edge 1 3) (edge 2 4)
         (edge 2 5))))

(define my-test-graph6
  (graph
   (list 0 1 2 3 4 5)
   (list (edge 0 1) (edge 0 2)
         (edge 1 3) (edge 1 4)
         (edge 2 5) (edge 5 3))))

;; otwarcie komponentu stosu
;(define-values/invoke-unit/infer bag-stack@)
;; opcja 2: otwarcie komponentu kolejki
(define-values/invoke-unit/infer bag-fifo@)

;; testy w Quickchecku
(require quickcheck)

;; test przykładowy: jeśli do pustej struktury dodamy element
;; i od razu go usuniemy, wynikowa struktura jest pusta
(quickcheck
 (property ([s arbitrary-symbol])
           (bag-empty? (bag-remove (bag-insert empty-bag s)))))
;; TODO: napisz inne własności do sprawdzenia!

; element włożony do pustej struktury jest tym samym elementem jaki dostaniemy po wyciągnięciu
(quickcheck
 (property ([s arbitrary-symbol])
           (eq? s (bag-peek (bag-insert empty-bag s)))))

;; pomocnicze funkcje dodające elementy z listy do struktury / wyjmujące elementy ze struktury
(define (push-list ls)
  (define (helper ls bag)
    (if (null? ls)
        bag
        (bag-insert (helper (cdr ls) bag) (car ls))))
  (helper (reverse ls) empty-bag))

(define (pop-list bag)
  (if (bag-empty? bag)
      '()
      (cons (bag-peek bag)
            (pop-list (bag-remove bag)))))

;; DLA KOLEJKI - sprawdzanie, czy elementy są poprawnie dodawane i usuwane
(quickcheck
 (property ([ls (arbitrary-list arbitrary-string)])
           (equal? (pop-list (push-list ls)) ls)))

;; DLA STOSU - sprawdzanie, czy elementy są dodawana i usuwane w poprawnej kolejności
#|(quickcheck
 (property ([ls (arbitrary-list arbitrary-string)])
           (equal? (reverse (pop-list (push-list ls))) ls)))|#

; DLA STOSU - nowy element powinien być na początku stosu 
#|(quickcheck
 (property ([s arbitrary-symbol]
            [r arbitrary-symbol]
            [p arbitrary-symbol])
           (let ([bag (bag-insert (bag-insert empty-bag r) p)])
             (eq? s (bag-peek (bag-insert bag s))))))|#

; DLA STOSU - pierwszy element powinien być tym, który został wstawiony najwcześniej
#|(quickcheck
 (property ([s arbitrary-symbol]
            [r arbitrary-symbol]
            [p arbitrary-symbol])
           (let ([bag (bag-insert (bag-insert empty-bag r) p)])
             (eq? r (bag-peek (bag-insert bag s))))))|#

; DLA KOLEJKI - najnowszy element powinien znajdować się na końcu kolejki
#|(quickcheck
 (property ([s arbitrary-symbol]
            [r arbitrary-symbol]
            [p arbitrary-symbol])
           (eq? p (bag-peek (bag-remove (bag-remove
                                         (bag-insert
                                          (bag-insert
                                           (bag-insert empty-bag s) r) p)))))))|#

; DLA KOLEJKI - najstarszy element powinien znajdować się na początku kolejki
#|(quickcheck
 (property ([s arbitrary-symbol]
            [r arbitrary-symbol]
            [p arbitrary-symbol])
           (eq? s (bag-peek (bag-insert (bag-insert (bag-insert empty-bag s) r) p)))))|#
            
;; jeśli jakaś własność dotyczy tylko stosu lub tylko kolejki,
;; wykomentuj ją i opisz to w komentarzu powyżej własności

;; otwarcie komponentu przeszukiwania
(define-values/invoke-unit/infer graph-search@)

;; uruchomienie przeszukiwania na przykładowym grafie
(search test-graph 1)

(search my-test-graph1 'e)

(search my-test-graph2 2)

(search my-test-graph3 5)
(search my-test-graph3 'X)

(search my-test-graph4 1)
(search my-test-graph4 5)

(search my-test-graph5 0)
(search my-test-graph5 2)

(search my-test-graph6 0)
(search my-test-graph6 1)

;; TODO: uruchom przeszukiwanie na swoich przykładowych grafach!