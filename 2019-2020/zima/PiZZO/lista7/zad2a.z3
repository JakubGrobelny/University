(declare-const p Bool)
(declare-const q Bool)

(assert (and (or p q) (or (not p) (not q)) (or p (not q))))
(check-sat)
(get-model)

