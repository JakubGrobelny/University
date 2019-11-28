(declare-const x0 Int)
(declare-const x1 Int)
(declare-const x2 Int)
(declare-const x3 Int)
(declare-const x4 Int)
(declare-const x5 Int)
(declare-const x6 Int)
(declare-const x7 Int)
(declare-const x8 Int)
(declare-const x9 Int)

(assert (and
    (or (= x0 0) (= x0 2))
    (or (= x1 0) (= x1 5))
    (or (= x2 0) (= x2 7))
    (or (= x3 0) (= x3 12))
    (or (= x4 0) (= x4 15))
    (or (= x5 0) (= x5 19))
    (or (= x6 0) (= x6 -4))
    (or (= x7 0) (= x7 -7))
    (or (= x8 0) (= x8 -18))
    (or (= x9 0) (= x9 -24))))

(assert (not
    (and
	(= x0 0)
	(= x1 0)
	(= x2 0)
	(= x3 0)
	(= x4 0)
	(= x5 0)
	(= x6 0)
	(= x7 0)
	(= x8 0)
	(= x9 0))))

(assert (= 0
    (+ x0 x1 x2 x3 x4 x5 x6 x7 x8 x9)))

(check-sat)
(get-model)
