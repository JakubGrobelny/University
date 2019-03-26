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
