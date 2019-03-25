revall(Xss, Yss) :-
    is_list(Xss),
    !,
    revall(Xss, Yss, []).
revall(X, X).

revall([], Acc, Acc) :-
    !.
revall([Xs | Xss], Yss, Acc) :-
    revall(Xs, Zs),
    revall(Xss, Yss, [Zs | Acc]),
    !.
