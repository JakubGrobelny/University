friends(my_cat, me).

likes(X, Y) :- friends(X, Y).
likes(X, Y) :- friends(Y, X).
likes(B, W) :- bird(B), worm(W).
likes(C, F) :- cat(C), fish(F).

cat(my_cat).

eats(my_cat, Y) :- likes(my_cat, Y).