#lang racket

;; pomocnicza funkcja dla list tagowanych o określonej długości

(define (tagged-tuple? tag len p)
  (and (list? p)
       (= (length p) len)
       (eq? (car p) tag)))

(define (tagged-list? tag p)
  (and (pair? p)
       (eq? (car p) tag)
       (list? (cdr p))))

;;
;; WHILE
;;
; memory

(define empty-mem
  null)

(define (set-mem x v m)
  (cond [(null? m)
         (list (cons x v))]
        [(eq? x (caar m))
         (cons (cons x v) (cdr m))]
        [else
         (cons (car m) (set-mem x v (cdr m)))]))

(define (get-mem x m)
  (cond [(null? m) 0]
        [(eq? x (caar m)) (cdar m)]
        [else (get-mem x (cdr m))]))

; arith and bool expressions: syntax and semantics

(define (const? t)
  (number? t))

(define (true? t)
  (eq? t 'true))

(define (false? t)
  (eq? t 'false))

(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * / = > >= < <= not and or mod rand))))

(define (op-op e)
  (car e))

(define (op-args e)
  (cdr e))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]
        [(eq? op '=) =]
        [(eq? op '>) >]
        [(eq? op '>=) >=]
        [(eq? op '<)  <]
        [(eq? op '<=) <=]
        [(eq? op 'not) not]
        [(eq? op 'and) (lambda x (andmap identity x))]
        [(eq? op 'or) (lambda x (ormap identity x))]
        [(eq? op 'mod) modulo]))

(define (var? t)
  (symbol? t))

;; procedura sprawdzająca, czy wyrażenie jest operatorem 'rand'
;; (jest on traktowany jako oddzielny przypadek)
(define (rand? e)
  (eq? (op-op e) 'rand))

;; pomocnicza procedura tworząca listę wartości argumentów oraz
;; ziaren, które powstały po ich obliczeniu
(define (eval-args args m)
  (if (null? args)
      null
      (let* ([evaluated (eval-arith (car args) m)]
             [seed (cdr evaluated)]
             [val (car evaluated)]
             [mem (set-mem seed-variable seed m)])
        (cons (cons val seed) (eval-args (cdr args) mem)))))

;; zmodyfikowana procedura eval-arith. Zwraca parę (wartość . ziarno)
(define (eval-arith e m)
  (if (op? e)
      ;; operator
      (if (rand? e)
          ;; operator rand
          (let* ([max  (eval-arith (car (op-args e)) m)]
                 [seed (cdr max)]
                 [rnd  ((rand (car max)) seed)]
                 [new-seed (cdr rnd)]
                 [result   (car rnd)])
            (cons result new-seed))
          ;; inny operator
          (let* ([args (eval-args (op-args e) m)]
                 [latest-seed (cdr (last args))]
                 [val (apply (op->proc (op-op e))
                             (map (lambda (x) (car x)) args))])
            (cons val latest-seed)))
      ;; stała/zmienna
      (let ([result
             (cond [(true? e) true]
                   [(false? e) false]
                   [(var? e) (get-mem e m)]
                   [(const? e) e])])
        (cons result (get-mem seed-variable m)))))

;; syntax of commands

(define (assign? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (second t) ':=)))

(define (assign-var e)
  (first e))

(define (assign-expr e)
  (third e))

(define (if? t)
  (tagged-tuple? 'if 4 t))

(define (if-cond e)
  (second e))

(define (if-then e)
  (third e))

(define (if-else e)
  (fourth e))

(define (while? t)
  (tagged-tuple? 'while 3 t))

(define (while-cond t)
  (second t))

(define (while-expr t)
  (third t))

(define (block? t)
  (list? t))

;; state

(define (res v s)
  (cons v s))

(define (res-val r)
  (car r))

(define (res-state r)
  (cdr r))

;; psedo-random generator

(define initial-seed
  123456789)

(define (rand max)
  (lambda (i)
    (let ([v (modulo (+ (* 1103515245 i) 12345) (expt 2 32))])
      (res (modulo v max) v))))

;; Language extensions

    ;; FOR
(define (for? expr)
  (and (list? expr)
       (= (length expr) 5)
       (eq? 'for (first expr))))

(define (for-assignment expr)
  (second expr))

(define (for-cond expr)
  (third expr))

(define (for-action expr)
  (fourth expr))

(define (for-expr expr)
  (fifth expr))

    ;; ++/--

(define (++? expr)
  (and (list? expr)
       (= 2 (length expr))
       (eq? '++ (first expr))
       (var? (second expr))))

(define (++var expr)
  (second expr))

(define (--? expr)
  (and (list? expr)
       (= 2 (length expr))
       (eq? '-- (first expr))
       (var? (second expr))))

(define (--var expr)
  (second expr))

;; wartość pod jaką w pamięci będzie znajdować się ziarno generatora liczb pseudolosowych
(define seed-variable '())

;; WHILE interpreter

;; (old-eval nie jest oryginalną procedurą old-eval tylko jej zmodyfikowaną wersją)
(define (old-eval e m)
  (printf "~a\n" m) ; for debugging
  (cond [(assign? e)
         (let* ([var (assign-var e)]
                [evaluated-expr (eval-arith (assign-expr e) m)]
                [val (car evaluated-expr)]
                [seed (cdr evaluated-expr)])
           (set-mem var val (set-mem seed-variable seed m)))]
        [(for? e)
         (old-eval (list 'while (for-cond e) (list (for-expr e) (for-action e)))
                   (old-eval (for-assignment e) m))]
        [(++? e)
         (let* ([var (++var e)]
                [val (get-mem var m)])
           (set-mem var (+ val 1) m))]
        [(--? e)
         (let* ([var (--var e)]
                [val (get-mem var m)])
           (set-mem var (- val 1) m))]
        [(if? e)
         (let* ([cnd (eval-arith (if-cond e) m)]
                [val (car cnd)]
                [seed (cdr cnd)]
                [mem (set-mem seed-variable seed m)])
           (if val
               (old-eval (if-then e) mem)
               (old-eval (if-else e) mem)))]
        [(while? e)
         (let* ([cnd (eval-arith (while-cond e) m)]
                [val (car cnd)]
                [seed (cdr cnd)]
                [mem (set-mem seed-variable seed m)])
           (if val
               (old-eval e (old-eval (while-expr e) mem))
               mem))]
        [(block? e)
         (if (null? e)
             m
             (old-eval (cdr e) (old-eval (car e) m)))]))

;; ZAD B

;; (procedura eval dalej wykorzystuje starą procedurę old-eval, która została zmodyfikowana)
(define (eval e m seed)
  ; ziarno jest przechowywane w pamięci pod wartością (zdefiniowaną wyżej jako seed-variable),
  ; której nie można nadpisać
  (let ([initial-mem (set-mem seed-variable seed empty-mem)])
  ; zwracana jest pamięć bez ziarna
    (cdr (old-eval e initial-mem))))

(define (run e)
  (eval e empty-mem initial-seed))

;; ZAD A
(define fermat-test
  '{(composite := false)
    (for (i := 0) (> k i) (++ i)
    {
      (a := (+ 2 (rand (- n 2))))
      (for ((pow := 0) (a^n := 1)) (> (- n 1) pow) (++ pow)
        {
          a^n := (* a^n a)
        })
      (if (not (= 1 (mod a^n n)))
          {
            (composite := true)
            (i := k)
          }
          {
            (composite := falsE)
          })
    })   
  })

(define (probably-prime? n k) ; check if a number n is prime using
                              ; k iterations of Fermat's primality
                              ; test
  (let ([memory (set-mem 'k k
                (set-mem 'n n empty-mem))])
    (not (get-mem
           'composite
           (eval fermat-test memory initial-seed)))))











