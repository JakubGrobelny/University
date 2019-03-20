append([], []) :-
    !.
append([Xs | Xss], Ys) :-
    append(Xss, Zs),
    append(Xs, Zs, Ys).