male(adam).
male(john).
male(joshua).
male(mark).
male(david).

female(eve).
female(helen).
female(ivonne).
female(anna).

parent(adam, helen).
parent(adam, ivonne).
parent(adam, anna).
parent(eve, helen).
parent(eve, ivonne).
parent(eve, anna).
parent(john, joshua).
parent(helen, joshua).
parent(ivonne, david).
parent(mark, david).

sibling(X, Y) :-
    parent(Z, X),
    parent(Z, Y),
    X \= Y.

sister(X, Y) :-
    female(X),
    sibling(X, Y).

grandson(X, Y) :-
    parent(Y, Z),
    parent(Z, X).

cousin(X, Y) :-
    parent(Z, Y),
    parent(V, X),
    sibling(Z, V).

descendant(X, Y) :-
    parent(Y, X).
descendant(X, Y) :-
    parent(Y, Z),
    descendant(X, Z).

is_mother(X) :-
    parent(X, _),
    female(X).

is_father(X) :-
    parent(X, _),
    male(X).