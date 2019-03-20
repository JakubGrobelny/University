prefix([], _).
prefix([H | PT], [H | T]) :-
    prefix(PT, T).

suffix(X, X).
suffix(S, [_ | T]) :-
    suffix(S, T).

segment([], _).
segment(X, Y) :-
    X = [_ | _],
    suffix(S, Y),
    prefix(X, S).

sublist([], _).
sublist([H | T0], [H | T1]) :-
    sublist(T0, T1).
sublist(S, [_ | T]) :-
    sublist(S, T).
