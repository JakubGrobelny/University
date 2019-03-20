reverse(Xs, Ys) :- 
    reverse(Xs, [], Ys, Ys).

reverse([], Ys, Ys, []).
reverse([X | Xs], Acc, Ys, [_ | YsGuard]) :-
    reverse(Xs, [X | Acc], Ys, YsGuard).
