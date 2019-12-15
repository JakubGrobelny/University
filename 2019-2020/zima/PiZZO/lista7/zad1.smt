(declare-const d Int)
(declare-const y Int)
(declare-const m Int)
(declare-const g Int)
(declare-const l Int)
(declare-const a Int)
(declare-const s Int)
(declare-const o Int)

(assert (and (<= 1 d) (<= d 9)))
(assert (and (<= 1 y) (<= y 9)))
(assert (and (<= 1 m) (<= m 9)))
(assert (and (<= 1 g) (<= g 9)))
(assert (and (<= 1 l) (<= l 9)))
(assert (and (<= 1 a) (<= a 9)))
(assert (and (<= 1 s) (<= s 9)))
(assert (and (<= 1 o) (<= o 9)))

(declare-const dym Int)
(declare-const mgla Int)
(declare-const smog Int)

(assert (= dym  (+ (* 100 d) (* 10 y) m)))
(assert (= mgla (+ (* 1000 m) (* 100 g) (* 10 l) a)))
(assert (= smog (+ (* 1000 s) (* 100 m) (* 10 o) g)))

(assert (= (+ dym mgla) smog))

(check-sat)
(get-model)

