sum(X, Y, Z) :-
    number(X),
    number(Y),
    !,
    Z is X + Y.
sum(X, Y, Z) :-
    number(X),
    number(Z),
    !,
    Y is Z - X.
sum(X, Y, Z) :-
    number(Y),
    number(Z),
    !,
    X is Z - Y.
sum(X, Y, Z) :-
    number(X),
