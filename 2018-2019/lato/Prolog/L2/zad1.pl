% bin jrst niefajnie – powinno być rbin z akumulatorem, który będzie odwracał

next([]).
next([0 | X]) :-
    next(X).
next([1 | X]) :-
    next(X).

list([]).
list([_ | T]) :-
    list(T).

bin([0]).
bin([1 | X]) :-
    list(X),
    next(X).    

rbin([0]).
rbin(X) :-
    rbin(Y),
    incremented(X, Y).

incremented([1], []).
incremented([1 | T], [0 | T]).
incremented([0 | T0], [1 | T1]) :-
    incremented(T0, T1).
