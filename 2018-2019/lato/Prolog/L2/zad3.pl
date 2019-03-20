sperm([], []).
sperm(XS, [HP | TP]) :-
    select(HP, XS, YS),
    sperm(YS, TP).

insert(X, XS, YS) :-
    select(X, YS, XS).

iperm([], []).
iperm([H | T], P) :-
    iperm(T, P1),
    insert(H, P1, P).

