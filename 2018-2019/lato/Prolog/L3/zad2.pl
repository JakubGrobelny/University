unifies(X, Y) :-
    not(X \= Y).

count(_, [], 0) :-
    !.
count(Elem, [H | T], N) :-
    unifies(Elem, H),
    !,
    count(Elem, T, M),
    N is M + 1.
count(Elem, [_ | T], M) :-
    count(Elem, T, M).
