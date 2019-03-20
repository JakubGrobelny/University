can_unify(X, Y) :-
    not(X \= Y).

% unifiable

count(_, [], 0) :-
    !.
count(Elem, [H | T], N) :-
    unifiable(Elem, H, _),
    !,
    count(Elem, T, M),
    N is M + 1.
count(Elem, [_ | T], M) :-
    count(Elem, T, M).

