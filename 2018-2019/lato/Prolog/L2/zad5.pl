% QUICKSORT
partition([X | Xs], Pivot, [X | Ls], Rs) :-
    X @=< Pivot, 
    !,
    partition(Xs, Pivot, Ls, Rs).
partition([X | Xs], Pivot, Ls, [X | Rs]) :-
    partition(Xs, Pivot, Ls, Rs).
partition([], _, [], []).

quicksort([], T, T).
quicksort([X | Xs], T, Ys) :-
    partition(Xs, X, L, R),
    quicksort(L, [X | Rs], Ys),
    quicksort(R, T, Rs).

quicksort(Xs, Ys) :-
    quicksort(Xs, [], Ys).

% MERGESORT 
merge([], Xs, Xs) :- !.
merge(Xs, [], Xs) :- !.
merge([X|Xs],[Y|Ys],[X|Zs]) :-
    X @=< Y,
    !,
    merge(Xs,[Y|Ys],Zs).
merge([X|Xs],[Y|Ys],[Y|Zs]) :-
    merge([X|Xs],Ys,Zs).

split([], [], []).
split([X], [X], []).
split([X, Y], [X], [Y]).
split([X0, X1 | Xs], [X0 | Ys], [X1 | Zs]) :-
    split(Xs, Ys, Zs).

mergesort([], []).
mergesort([X], [X]).
mergesort(Xs, Sorted) :-
    split(Xs, Ys, Zs),
    mergesort(Ys, YsSorted),
    mergesort(Zs, ZsSorted),
    merge(YsSorted, ZsSorted, Sorted).

% HEAPSORT
% heap(nil).
% heap(tree(_, nil, nil)).
% heap(tree(V, tree(LV, LL, LR), tree(RV, RL, RR))) :-
%     heap(tree(LV, LL, LR)),
%     heap(tree(RV, RL, RR)),
%     LV @=< V,
%     RV @=< V.
% heap(tree(V, nil, tree(RV, RL, RR))) :-
%     heap(tree(RV, RL, RR)),
%     RV @=< V.
% heap(tree(V, tree(LV, LL, LR), nil)) :-
%     heap(tree(LV, LL, LR)),
%     LV @=< V.

% heapsort(Xs, Ys) :-
%     heap_of_list(H, Xs),
%     list_of_heap(Ys, H).

% heap_of_list(nil, []).
% heap_of_list(Heap, [H | T]) :-
%     heap_insert(H, Heap0, Heap),
%     heap_of_list(T, Heap0).

% list_of_heap([], nil).
% list_of_heap([H | T], Heap) :-
%     heap_insert(H, )
