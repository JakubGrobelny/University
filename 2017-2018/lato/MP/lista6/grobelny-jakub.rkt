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

;; reprezentacja danych wejściowych (z ćwiczeń)
(define (var? x)
  (symbol? x))

(define (var x)
  x)

(define (var-name x)
  x)

;; przydatne predykaty na zmiennych
(define (var<? x y)
  (symbol<? x y))

(define (var=? x y)
  (eq? x y))

(define (literal? x)
  (and (tagged-tuple? 'literal 3 x)
       (boolean? (cadr x))
       (var? (caddr x))))

(define (literal pol x)
  (list 'literal pol x))

(define (literal-pol x)
  (cadr x))

(define (literal-var x)
  (caddr x))

(define (clause? x)
  (and (tagged-list? 'clause x)
       (andmap literal? (cdr x))))

(define (clause . lits)
  (cons 'clause lits))

(define (clause-lits c)
  (cdr c))

(define (cnf? x)
  (and (tagged-list? 'cnf x)
       (andmap clause? (cdr x))))

(define (cnf . cs)
    (cons 'cnf cs))

(define (cnf-clauses x)
  (cdr x))

;; oblicza wartość formuły w CNF z częściowevalym wartościowaniem. jeśli zmienna nie jest
;; zwartościowana, literał jest uznawany za fałszywy.
(define (valuate-partial val form)
  (define (val-lit l)
    (let ((r (assoc (literal-var l) val)))
      (cond
       [(not r)  false]
       [(cadr r) (literal-pol l)]
       [else     (not (literal-pol l))])))
  (define (val-clause c)
    (ormap val-lit (clause-lits c)))
  (andmap val-clause (cnf-clauses form)))

;; reprezentacja dowodów sprzeczności

(define (axiom? p)
  (tagged-tuple? 'axiom 2 p))

(define (proof-axiom c)
  (list 'axiom c))

(define (axiom-clause p)
  (cadr p))

(define (res? p)
  (tagged-tuple? 'resolve 4 p))

(define (proof-res x pp pn)
  (list 'resolve x pp pn))

(define (res-var p)
  (cadr p))

(define (res-proof-pos p)
  (caddr p))

(define (res-proof-neg p)
  (cadddr p))

;; sprawdza strukturę, ale nie poprawność dowodu
(define (proof? p)
  (or (and (axiom? p)
           (clause? (axiom-clause p)))
      (and (res? p)
           (var? (res-var p))
           (proof? (res-proof-pos p))
           (proof? (res-proof-neg p)))))

;; procedura sprawdzająca poprawność dowodu
(define (check-proof pf form)
  (define (run-axiom c)
    (displayln (list 'checking 'axiom c))
    (and (member c (cnf-clauses form))
         (clause-lits c)))
  (define (run-res x cpos cneg)
    (displayln (list 'checking 'resolution 'of x 'for cpos 'and cneg))
    (and (findf (lambda (l) (and (literal-pol l)
                                 (eq? x (literal-var l))))
                cpos)
         (findf (lambda (l) (and (not (literal-pol l))
                                 (eq? x (literal-var l))))
                cneg)
         (append (remove* (list (literal true x))  cpos)
                 (remove* (list (literal false x)) cneg))))
  (define (run-proof pf)
    (cond
     [(axiom? pf) (run-axiom (axiom-clause pf))]
     [(res? pf)   (run-res (res-var pf)
                           (run-proof (res-proof-pos pf))
                           (run-proof (res-proof-neg pf)))]
     [else        false]))
  (null? (run-proof pf)))


;; reprezentacja wewnętrzna

;; sprawdza posortowanie w porządku ściśle rosnącym, bez duplikatów
(define (sorted? vs)
  (or (null? vs)
      (null? (cdr vs))
      (and (var<? (car vs) (cadr vs))
           (sorted? (cdr vs)))))

(define (sorted-varlist? x)
  (and (list? x)
       (andmap var? x)
       (sorted? x)))

;; klauzulę reprezentujemy jako parę list — osobno wystąpienia pozytywne i negatywne. Dodatkowo
;; pamiętamy wyprowadzenie tej klauzuli (dowód) i jej rozmiar.
(define (res-clause? x)
  (and (tagged-tuple? 'res-int 5 x)
       (sorted-varlist? (second x))
       (sorted-varlist? (third x))
       (= (fourth x) (+ (length (second x)) (length (third x))))
       (proof? (fifth x))))

(define (res-clause pos neg proof)
  (list 'res-int pos neg (+ (length pos) (length neg)) proof))

(define (res-clause-pos c)
  (second c))

(define (res-clause-neg c)
  (third c))

(define (res-clause-size c)
  (fourth c))

(define (res-clause-proof c)
  (fifth c))

;; przedstawia klauzulę jako parę list zmiennych występujących odpowiednio pozytywnie i negatywnie
(define (print-res-clause c)
  (list (res-clause-pos c) (res-clause-neg c)))

;; sprawdzanie klauzuli sprzecznej
(define (clause-false? c)
  (and (null? (res-clause-pos c))
       (null? (res-clause-neg c))))

;; pomocnicze procedury: scalanie i usuwanie duplikatów z list posortowanych
(define (merge-vars xs ys)
  (cond [(null? xs) ys]
        [(null? ys) xs]
        [(var<? (car xs) (car ys))
         (cons (car xs) (merge-vars (cdr xs) ys))]
        [(var<? (car ys) (car xs))
         (cons (car ys) (merge-vars xs (cdr ys)))]
        [else (cons (car xs) (merge-vars (cdr xs) (cdr ys)))]))

(define (remove-duplicates-vars xs)
  (cond [(null? xs) xs]
        [(null? (cdr xs)) xs]
        [(var=? (car xs) (cadr xs)) (remove-duplicates-vars (cdr xs))]
        [else (cons (car xs) (remove-duplicates-vars (cdr xs)))]))

(define (rev-append xs ys)
  (if (null? xs) ys
      (rev-append (cdr xs) (cons (car xs) ys))))

;; TODO: miejsce na uzupełnienie własnych funkcji pomocniczych

;; procedura sprawdzająca, czy klauzula c1 jest ”łatwiejsza” od c2
(define (is-easier? c1 c2)
  (let ([pos-diff (remove* (res-clause-pos c1) (res-clause-pos c2))]
        [neg-diff (remove* (res-clause-neg c1) (res-clause-neg c2))])
    (and (null? pos-diff) (null? neg-diff))))

;; procedura sprawdzająca, czy klauzula c jest "łatwiejsza" od którejkolwiek
;; klauzuli z listy clauses.
(define (is-easier-than-any? c clauses)
  (cond [(null? clauses) false]
        [(is-easier? c (car clauses)) true]
        [else (is-easier-than-any? c (cdr clauses))]))

;; procedura usuwająca ze zbioru klauzul te, które są "łatwiejsze"
(define (remove-easier ls clause)
  (cond [(null? ls) null]
        [(is-easier? clause (car ls)) (cdr ls)]
        [else (cons (car ls) (remove-easier (cdr ls) clause))]))

;; procedura sprawdzająca, czy lista ls zawiera zmienną var
(define (contains-var var ls)
  (if (null? ls)
      false
      (if (eq? var (car ls))
          true
          (contains-var var (cdr ls)))))

;; procedura szukająca zanegowanych zmiennych z listy pos w liście neg
(define (find-negations pos neg)
  (if (null? pos)
      null
      (if (contains-var (car pos) neg)
          (car pos)
          (find-negations (cdr pos) neg))))

;; procedura usuwająca zmienna var z listy
(define (remove-var ls var)
  (if (null? ls)
      null
      (if (eq? (car ls) var)
          (cdr ls)
          (cons (car ls) (remove-var (cdr ls) var)))))
;; Koniec własnych funkcji pomocniczych

(define (clause-trivial? c)
  (not (null? (find-negations (res-clause-pos c) (res-clause-neg c)))))

(define (resolve c1 c2)
  ;; szukamy zanegowanych zmiennych w drugiej klauzuli, zaczynając od c1
  (let ([var1 (find-negations (res-clause-pos c1) (res-clause-neg c2))]
        [var2 (find-negations (res-clause-pos c2) (res-clause-neg c1))])
    (let ([var (if (null? var1) var2 var1)])
      ;; jeżeli zbiory się nie przecinają, to zwracamy fałsz
      (if (null? var)
          false
          ;; nowe zbiory zmiennych zanegowanych i niezanegowanych 
          (let ([new-pos (remove-var (remove-duplicates-vars (merge-vars
                                                              (res-clause-pos c1)
                                                              (res-clause-pos c2)))
                                     var)]
                [new-neg (remove-var (remove-duplicates-vars (merge-vars
                                                              (res-clause-neg c1)
                                                              (res-clause-neg c2)))
                                     var)])
            (let ([was-c1-pos? (if (null? var1) #f #t)])
              ;; wyprowadzenie nowej klauzuli
              (let ([new-proof (proof-res var (if was-c1-pos?
                                                  (res-clause-proof c1)
                                                  (res-clause-proof c2))
                                              (if was-c1-pos?
                                                   (res-clause-proof c2)
                                                   (res-clause-proof c1)))])
                ;; tworzenie nowej klauzuli
                (res-clause new-pos new-neg new-proof))))))))
  
(define (resolve-single-prove s-clause checked pending)
  ;; TODO: zaimplementuj!
  ;; procedura podstawiająca rezolwentę za klauzulę, jeżeli można zrezolwować ją z s-clause
  (define (change-to-resolution clause)
    (if (null? (res-clause-neg s-clause))
        (let ([var (car (res-clause-pos s-clause))])
          (if (contains-var var (res-clause-neg clause))
              (resolve s-clause clause)
              clause))
        (let ([var (car (res-clause-neg s-clause))])
          (if (contains-var var (res-clause-pos clause))
              (resolve s-clause clause)
              clause))))
  (let* ([new-checked (sort-clauses (cons s-clause (map (lambda (clause)
                                                         (change-to-resolution clause)) checked)))]
         [new-pending (sort-clauses (map (lambda (clause)
                                           (change-to-resolution clause)) pending))])
    (resolve-prove new-checked new-pending)))

;; wstawianie klauzuli w posortowaną względem rozmiaru listę klauzul
(define (insert nc ncs)
  (cond
   [(null? ncs)                     (list nc)]
   [(< (res-clause-size nc)
       (res-clause-size (car ncs))) (cons nc ncs)]
   [else                            (cons (car ncs) (insert nc (cdr ncs)))]))

;; sortowanie klauzul względem rozmiaru (funkcja biblioteczna sort)
(define (sort-clauses cs)
  (sort cs < #:key res-clause-size))

;; główna procedura szukająca dowodu sprzeczności
;; zakładamy że w checked i pending nigdy nie ma klauzuli sprzecznej
(define (resolve-prove checked pending)
  (cond
   ;; jeśli lista pending jest pusta, to checked jest zamknięta na rezolucję czyli spełnialna
   [(null? pending) (generate-valuation (sort-clauses checked))]
   ;; jeśli klauzula ma jeden literał, to możemy traktować łatwo i efektywnie ją przetworzyć
   [(= 1 (res-clause-size (car pending)))
    (resolve-single-prove (car pending) checked (cdr pending))]
   ;; w przeciwnym wypadku wykonujemy rezolucję z wszystkimi klauzulami już sprawdzonymi, a
   ;; następnie dodajemy otrzymane klauzule do zbioru i kontynuujemy obliczenia
   [else
    (let* ((next-clause  (car pending))
           (rest-pending (cdr pending))
           (resolvents   (filter-map (lambda (c) (resolve c next-clause))
                                     checked))
           (sorted-rs    (sort-clauses resolvents)))
      (subsume-add-prove (cons next-clause checked) rest-pending sorted-rs))]))

;; procedura upraszczająca stan obliczeń biorąc pod uwagę świeżo wygenerowane klauzule i
;; kontynuująca obliczenia. Do uzupełnienia.
(define (subsume-add-prove checked pending new)
  (cond
   [(null? new)                 (resolve-prove checked pending)]
   ;; jeśli klauzula do przetworzenia jest sprzeczna to jej wyprowadzenie jest dowodem sprzeczności
   ;; początkowej formuły
   [(clause-false? (car new))   (list 'unsat (res-clause-proof (car new)))]
   ;; jeśli klauzula jest trywialna to nie ma potrzeby jej przetwarzać
   [(clause-trivial? (car new)) (subsume-add-prove checked pending (cdr new))]
   [else
    ;; TODO: zaimplementuj!
    (let ([clause (car new)])
      (if (or (is-easier-than-any? clause checked)
              (is-easier-than-any? clause pending))
          ;; jeżeli klauzula jest "łatwiejsza" od którejkolwiek z list checked lub pending, to nie ma potrzeby jej przetwarzać
          (subsume-add-prove checked pending (cdr new))
          ;; w przeciwnym razie usuwamy te klauzule, które są "łatwiejsze" od nowej
          (let ([new-checked (remove-easier checked clause)]
                [new-pending (insert clause (remove-easier pending clause))])
            (subsume-add-prove new-checked new-pending (cdr new)))))]))

(define (generate-valuation resolved)
  ;; procedura usuwająca klauzule, które są spełnione dla zmiennej var z wartościowaniem prawda/fałsz
  ;; oraz upraszczająca klauzule zawierająca zanegowane zmienne
  (define (simplify-clauses clauses var is-negated?)
    ;; procedura upraszczająca klauzule poprzez usunięcie 
    (define (simplify clause)
      (if is-negated?
          (let* ([pos (res-clause-pos clause)]
                 [new-pos (remove-var pos var)])
            (res-clause new-pos (res-clause-neg clause) (res-clause-proof clause)))
          (let* ([neg (res-clause-neg clause)]
                 [new-neg (remove-var neg var)])
            (res-clause (res-clause-pos clause) new-neg (res-clause-proof clause)))))
    ;; procedura sprawdzająca, czy dana klauzula jest spełniona
    (define (is-satisfied? clause)
      (let ([variables (if is-negated? (res-clause-neg clause) (res-clause-pos clause))])
        (cond [(null? variables) false]
              [(contains-var var variables) true]
              [else false])))
    (cond [(null? clauses) null]
          [(is-satisfied? (car clauses)) (cdr clauses)]
          [else (cons (simplify (car clauses)) (cdr clauses))]))
  ;; procedura pomocnicza generująca listę wartości zmiennych spełniających zbiór klauzul
  (define (generate-val-aux clauses valuation)
    (if (null? clauses)
        valuation
        (let* ([clause (car clauses)]
               [clause-neg (res-clause-neg clause)]
               [clause-pos (res-clause-pos clause)]
               [neg-len (length clause-neg)]
               [pos-len (length clause-pos)])
          ;; bez znaczenia jest to, ile elementów ma klauzula, ponieważ są posortowane rosnąco względem długości
          ;; i za każdym razem możemy wybrać dowolną zmienną, która ma być spełniona (w przypadku klauzul jednoelementowych
          ;; wybór jest trywialny.
          (let ([negated (if (> neg-len 0) #t #f)])
            (let ([var (if negated (car clause-neg) (car clause-pos))])
              (generate-val-aux (simplify-clauses (cdr clauses) var negated)
                                (cons (cons var (if negated #f #t)) valuation)))))))
  ;; klauzule są dodatkowo sortowane, na wypadek gdyby na wejściu podano je w złej kolejności (nierosnącej)
  (list 'sat (generate-val-aux (sort-clauses resolved) '())))
              
                 
;; procedura przetwarzające wejściowy CNF na wewnętrzną reprezentację klauzul
(define (form->clauses f)
  (define (conv-clause c)
    (define (aux ls pos neg)
      (cond
       [(null? ls)
        (res-clause (remove-duplicates-vars (sort pos var<?))
                    (remove-duplicates-vars (sort neg var<?))
                    (proof-axiom c))]
       [(literal-pol (car ls))
        (aux (cdr ls)
             (cons (literal-var (car ls)) pos)
             neg)]
       [else
        (aux (cdr ls)
             pos
             (cons (literal-var (car ls)) neg))]))
    (aux (clause-lits c) null null))
  (map conv-clause (cnf-clauses f)))

(define (prove form)
  (let* ((clauses (form->clauses form)))
    (subsume-add-prove '() '() clauses)))

;; procedura testująca: próbuje dowieść sprzeczność formuły i sprawdza czy wygenerowany
;; dowód/waluacja są poprawne. Uwaga: żeby działała dla formuł spełnialnych trzeba umieć wygenerować
;; poprawną waluację.
(define (prove-and-check form)
  (let* ((res (prove form))
         (sat (car res))
         (pf-val (cadr res)))
    (if (eq? sat 'sat)
        (valuate-partial pf-val form)
        (check-proof pf-val form))))

;;; TODO: poniżej wpisz swoje testy

(display "generate-valuation:\n\n")

(define c1_1 (res-clause (list 'p 'q) (list 'r) 'axiom))
(define c1_2 (res-clause '() (list 'p 'r) 'axiom))
(define c1_3 (res-clause (list 'q 's) '() 'axiom))
(define c1_4 (res-clause (list 't) '() 'axiom))
(define clauses1 (list c1_1 c1_2 c1_3 c1_4))
(display "(p ∨ q ∨ ¬r) ∧ (¬p ∨ ¬r) ∧ (q ∨ s) ∧ (t):\n")
(generate-valuation clauses1)
(display "\n")

(define c2_1 (res-clause (list 'a) (list 'b 'c 'd) 'axiom))
(define c2_2 (res-clause (list 'b 'c) (list 'd 'e) 'axiom))
(define c2_3 (res-clause '() (list 'c) 'axiom))
(define clauses2 (list c2_1 c2_2 c2_3))
(display "(a ∨ ¬b ∨ ¬c ∨ ¬d) ∧ (b ∨ c ∨ ¬d ∨ ¬ e) ∧ (¬c):\n")
(generate-valuation clauses2)
(display "\n")

(define c3_1 (res-clause (list 'p) '() 'axiom))
(define c3_2 (res-clause (list 'r) (list 'p) 'axiom))
(define c3_3 (res-clause (list 'q 's) (list 'r) 'axiom))
(define c3_4 (res-clause (list 'q) (list 'r 's) 'axiom))
(define clauses3 (list c3_1 c3_2 c3_3 c3_4))
(display "(p) ∧ (r ∨ ¬p) ∧ (q ∨ s ∨ ¬r) ∧ (q ∨ ¬r ∨ ¬s):\n")
(generate-valuation clauses3)
(display "\n")

(define c4_1 (res-clause (list 'a 'b) '() 'axiom))
(define c4_2 (res-clause (list 'c) (list 'b 'd) 'axiom))
(define c4_3 (res-clause (list 'd) (list 'e) 'axiom))
(define clauses4 (list c4_1 c4_2 c4_3))
(display "(a ∨ b) ∧ (¬b ∨ c ∨ ¬d) ∧ (d ∨ ¬e):\n")
(generate-valuation clauses4)
(display "\n")

(display "resolve:\n\n")

c2_2 c2_1
(resolve c2_2 c2_1)
(display "\n")

c3_1 c3_2
(resolve c3_1 c3_2)
(display "\n")

c4_1 c4_2
(resolve c4_1 c4_2)
(display "\n")

c1_1 c1_2
(resolve c1_1 c1_2)
(display "\n")

(display "proves (the same clauses as in generate-valuation tests):\n\n")
(resolve-prove '() clauses1)
(resolve-prove '() clauses2)
(resolve-prove '() clauses3)
(resolve-prove '() clauses4)
 