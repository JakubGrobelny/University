append([], []) :-
    !.
append([L | Ls], Xs) :-
    append(Ls, Ys),
    append(L, Ys, Xs).