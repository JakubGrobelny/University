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

split([], []) :-
    !.
split([X | Xs], [[X] | Ys]) :-
split(Xs, Ys).

merge_sort([], []) :-
    !.
merge_sort(Xs, Ys) :-
    split(Xs, Zs),
    merge_sort_bottom_up(Zs, Ys), 
    !.

merge_pairs([], []) :-
    !.
merge_pairs([Xs], [Xs]).
merge_pairs([Xs, Ys | T], [Vs | Zs]) :-
    merge(Xs, Ys, Vs),
    merge_pairs(T, Zs).

merge_sort_bottom_up([Xs], Xs) :-
    !.
merge_sort_bottom_up(Xss, Ys) :-
    merge_pairs(Xss, Yss),
    merge_sort_bottom_up(Yss, Ys).

