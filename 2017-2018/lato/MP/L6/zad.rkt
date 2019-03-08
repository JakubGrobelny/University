#lang racket

(define (const? t)
  (number? t))

(define (binop? t)
  (and (list? t)
       (= (length t) 3)
       (member (car t) '(+ - * /))))

(define (binop-op e)
  (car e))

(define (binop-left e)
  (cadr e))

(define (binop-right e)
  (caddr e))

(define (binop-cons op l r)
  (list op l r))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]))

(define (let-def? t)
  (and (list? t)
       (= (length t) 2)
       (symbol? (car t))))

(define (let-def-var e)
  (car e))

(define (let-def-expr e)
  (cadr e))

(define (let-def-cons x e)
  (list x e))

(define (let? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'let)
       (let-def? (cadr t))))

(define (let-def e)
  (cadr e))

(define (let-expr e)
  (caddr e))

(define (let-cons def e)
  (list 'let def e))

(define (var? t)
  (symbol? t))

(define (var-var e)
  e)

(define (var-cons x)
  x)

(define (hole? t)
  (eq? t 'hole))

(define (arith/let/holes? t)
  (or (hole? t)
      (const? t)
      (and (binop? t)
           (arith/let/holes? (binop-left  t))
           (arith/let/holes? (binop-right t)))
      (and (let? t)
           (arith/let/holes? (let-expr t))
           (arith/let/holes? (let-def-expr (let-def t))))
      (var? t)))

(define (num-of-holes t)
  (cond [(hole? t) 1]
        [(const? t) 0]
        [(binop? t)
         (+ (num-of-holes (binop-left  t))
            (num-of-holes (binop-right t)))]
        [(let? t)
         (+ (num-of-holes (let-expr t))
            (num-of-holes (let-def-expr (let-def t))))]
        [(var? t) 0]))

(define (arith/let/hole-expr? t)
  (and (arith/let/holes? t)
       (= (num-of-holes t) 1)))

(define (hole-context e)
  ;; predykat sprawdzający, czy zmienna była już zdefiniowana (aby uniknąć powtarzających się zmiennych)
  (define (exists-in-context? var context)
    (if (null? context)
        false
        (if (eq? var (car context))
            true
            (exists-in-context? var (cdr context)))))
  ;; procedura iterująca się przez wyrażenie tworząca listę związanych zmiennych
  (define (hole-context-iter e context)
    (cond [(not (arith/let/hole-expr? e)) context]
          [(binop? e)
                  (if (arith/let/hole-expr? (binop-op e))
                      (hole-context-iter (binop-op e) context)
                      (if (arith/let/hole-expr? (binop-left e))
                          (hole-context-iter (binop-left e) context)
                          (hole-context-iter (binop-right e) context)))]
          [(let? e)
           (if (hole? (let-def-expr (let-def e)))
                    context
                    (let ([var (let-def-var (let-def e))]
                          [in-expr? (arith/let/hole-expr? (let-expr e))])
                      (let ([expr (if in-expr?
                                      (let-expr e)
                                      (let-def-expr (let-def e)))])
                        (if (or (exists-in-context? var context)
                                (not in-expr?))
                            (hole-context-iter expr context)
                            (hole-context-iter expr (cons var context))))))]
          [else context]))
  (hole-context-iter e '()))
      
(define (test)
  ;; procedura porównująca dwa konteksty
  (define (is-correct? context correct-context)
    (and (= (length context)
            (length correct-context))
         (let ([diff (remove* correct-context context)])
           (null? diff))))
  ;; testy
  (values
   (is-correct? (hole-context '(42))
                '())
   (is-correct? (hole-context '(+ hole 2))
                '())
   (is-correct? (hole-context '(let (x hole) (* x x)))
                '())
   (is-correct? (hole-context '(let (x 4) (let (y hole) (+ y x))))
                '(x))
   (is-correct? (hole-context '(let (a 1)
                               (let (b 11)
                               (let (c 111)
                               (let (d 1111)
                                 (+ hole (+ a (+ b (+ c d)))))))))
                '(a b c d))
   (is-correct? (hole-context '(let (x (let (y 20) (+ y 10)))
                                 (+ (+ x 12) hole)))
                '(x))
   (is-correct? (hole-context '(let (x (let (y 20) (+ y hole))) x))
                '(y))
   (is-correct? (hole-context '(let (x (let (y hole) (* y 2))) (/ x 0)))
                '())
   (is-correct? (hole-context '(let (x (let (y (let (z 2)
                                                 (+ z hole)))
                                         (+ y 2)))
                                 (+ x 42)))
                '(z))
   (is-correct? (hole-context '(let (x 1)
                               (let (y 2)
                               (let (z 3)
                               (let (xyz hole)
                                 (* xyz (+ x (+ y z))))))))
                '(z y x))
   (is-correct? (hole-context '(+ 3 (let (x 2) (* hole x))))
                '(x))
   (is-correct? (hole-context '(* (+ (let (x (let (y 3) (+ (let (z 4) (* hole z)) 7))) (* x x)) 3) 1))
                '(y z))))
   





  