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
male(adam).
male(john).
male(david).
male(mark).
male(joshua).
female(eve).
female(helen).
female(ivonne).
female(anna).

sibling(X, Y) :- 
    parent(Z, X), 
    parent(Z, Y).

sister(X, Y) :- 
    sibling(X, Y), 
    female(X).

grandson(X, Y) :- 
    male(X), 
    parent(Z, X), 
    parent(Y, Z).

cousin(X, Y) :- 
    parent(Y, Z), 
    parent(X, V), 
    sibling(Z, V).

is_mother(X) :- 
    female(X), 
    parent(X, _).

is_father(X) :- 
    male(X), 
    parent(X, _).

descendant(X, Y) :- 
    parent(Y, X).
descendant(X, Y) :- 
    parent(Y, Z), 
    descendant(X, Z).