(push)
(assert
    (exists ((p Bool))
	(exists ((q Bool))
	    (and (or p q) (or (not p) (not q)) (or p (not q))))))
(check-sat)
(get-model)
(pop)

(push)
(assert 
    (forall ((p Bool))
	(forall ((q Bool))
	    (and (or p q) (or (not p) (not q)) (or p (not q))))))
(check-sat)
(get-model)
(pop)
