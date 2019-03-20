list([]).
list([_H | T]) :- 
    list(T).

appended([], Y, Y).
appended([H | T], Y, [H | X]) :-
    appended(T, Y, X).
