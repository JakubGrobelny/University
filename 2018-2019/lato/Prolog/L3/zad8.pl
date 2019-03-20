merge_sort(Xs, Ys) :-
    length(Xs, N),
    merge_sort(Xs, N, Ys, []).

merge_sort(Xs, 0, [], Xs) :-
    !.
merge_sort([X | T], 1, [X], T) :-
    !.
merge_sort(Xs, N, Ys, T) :-
    NHalf is N // 2,
    NRest is N - NHalf,
    merge_sort(Xs, NHalf, HalfSorted, Rest),
    merge_sort(Rest, NRest, RestSorted, T),
    merge(HalfSorted, RestSorted, Ys).


merge([], X, X) :-
    !.
merge(X, [], X) :-
    !.
merge([H | T], S, [H | R]) :-
    S = [G | _],
    H @=< G,
    !,
    merge(T, S, R).
merge(T, [H | S], [H | R]) :-
    merge(T, S, R).