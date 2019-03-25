flatten([], []) :-
    !.
flatten([Xs | Xss], Ys) :-
    is_list(Xs),
    !,
    flatten(Xs, Zs),
    flatten(Xss, Vs),
    append(Zs, Vs, Ys).
flatten([X | Xss], [X | Zs]) :-
    flatten(Xss, Zs).