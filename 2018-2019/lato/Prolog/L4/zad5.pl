is_valid(N, M, pos(X, Y)) :-
    X =< N,
    X >= 1,
    Y =< M,
    Y >= 1.

abs(X, Offset, Y) :-
    Y is X - Offset.
abs(X, Offset, Y) :-
    Y is X + Offset.

can_move(N, M, pos(FromX, FromY), pos(ToX, ToY)) :-
    (abs(FromX, 1, ToX),
     abs(FromY, 2, ToY),
     is_valid(N, M, pos(ToX, ToY)));
    (abs(FromX, 2, ToX),
     abs(FromY, 1, ToY),
     is_valid(N, M, pos(ToX, ToY))).

solve(N, M, pos(X, Y), Moves) :-
    Counter is N * M - 1,
    solve(N, M, pos(X, Y), RevMoves, [], Counter),
    reverse(RevMoves, Moves).

solve(_, _, pos(X, Y), [pos(X, Y) | Acc], Acc, 0) :- !.
solve(N, M, pos(X,Y), Moves, Acc, Counter) :-
    CounterNext is Counter - 1,
    can_move(N, M, pos(X, Y), pos(NewX, NewY)),
    \+ member(pos(NewX, NewY), Acc),
    solve(N, M, pos(NewX, NewY), Moves, [pos(X, Y) | Acc], CounterNext).
