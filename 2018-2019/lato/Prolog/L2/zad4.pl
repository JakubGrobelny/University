perm(X, Y) :- lists:perm(X, Y).monotone([]).
monotone([_]).
monotone([H0, H1 | T]) :-
    H0 @=< H1,
    monotone([H1 | T]).

insert(X, [], [X]).
insert(X, [H | T], [X, H | T]) :-
    X @=< H.
insert(X, [H | T], [H | T2]) :-
    X @>= H,
    insert(X, T, T2).
    
isort([], []).
isort([H | T], X) :-
    isort(T, L),
    insert(H, L, X).

select_min([X], X, []).
select_min([H | T], H, T) :-
    select_min(T, MT, _),
    H @=< MT.
select_min([H | T], MT, [H | S]) :-
    select_min(T, MT, S),
    H @>= MT.

ssort([], []).
ssort(X, [H | T]) :-
    select_min(X, H, Ys),
    ssort(Ys, T).
