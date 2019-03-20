eq_len([], []).
eq_len([_ | Xs], [_ | Ys]) :-
    eq_len(Xs, Ys).

sperm([], []).
sperm(XS, [HP | TP]) :-
    eq_len(XS, [HP | TP]),
    select(HP, XS, YS),
    sperm(YS, TP).

insert(X, XS, YS) :-
    select(X, YS, XS).

iperm(Xs, Ys) :-
    iperm(Xs, Ys, Ys).
iperm([], [], []).
iperm([H | T], P, [_ | Bound]) :-
    iperm(T, P1, Bound),
    insert(H, P1, P).

