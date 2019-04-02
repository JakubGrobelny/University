insert(X, leaf, node(leaf, X, leaf)) :-
    !.
insert(X, node(L, V, R), node(L0, V, R)) :-
    X @=< V,
    !,
    insert(X, L, L0).
insert(X, node(L, V, R), node(L, V, R0)) :-
    insert(X, R, R0),
    !.
insert(_, T, R) :-
    \+ var(T),
    \+ var(R),
    throw(domain_error).

minel(node(leaf, V, R), V, R) :-
    !.
minel(node(L, V, R), Min, node(L0, V, R)) :-
    minel(L, Min, L0).

maxel(node(L, V, leaf), V, L) :-
    !.
maxel(node(L, V, R), Max, node(L, V, R0)) :-
    maxel(R, Max, R0).

mirror(leaf, leaf) :-
    !.
mirror(node(L, V, R), node(L0, V, R0)) :-
    mirror(L, R0),
    mirror(R, L0).

flatten(leaf, []) :-
    !.
flatten(node(L, V, R), Xs) :-
    flatten(L, Ls),
    flatten(R, Rs),
    append(Ls, [V | Rs], Xs).

% Do testów
tree_of_list(leaf, []) :-
    !.
tree_of_list(T, [X | Xs]) :-
    tree_of_list(T0, Xs),
    insert(X, T0, T).
