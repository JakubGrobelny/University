append([], X, X).
append([H | T], X, [H | Y]) :-
    append(T, X ,Y).

select(H, [H | T], T).
select(X, [H | T], [H | S]) :-
    select(X, T, S).
