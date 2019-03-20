male(adam).
male(son(_, _, _)).

father(X, son(_, X, _)).
father(X, daughter(_, X, _)).

mother(Y, son(_, _, Y)).
mother(Y, daughter(_, _, Y)).

parent(X, Y) :- 
    father(X, Y).
parent(X, Y) :- 
    mother(X, Y).
