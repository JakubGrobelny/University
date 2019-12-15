(declare-const p Bool)
(declare-const q Bool)

(assert (and (or p q) (or (not p) q) (or (not q) p) (or (not p) (not q))))
(check-sat)
(get-model)
