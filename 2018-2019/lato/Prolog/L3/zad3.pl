len(Xs, N) :-
    len(Xs, 0, N).

% Przypadek kiedy długość jest ukonkretniona
len([], Removed, Length) :-
    integer(Length),
    Length =:= Removed,
    !.
len([], N, N).
len([_ | Xs], Removed, Length) :-
    NextRemoved is Removed + 1,
    len(Xs, NextRemoved, Length).
