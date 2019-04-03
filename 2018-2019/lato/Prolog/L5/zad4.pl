empty(stack([])).
empty(queue(Xs-_)) :-
    nonvar(Xs),
    !,
    fail.
empty(queue(Tail-Tail)).


put(E, stack(Xs), stack([E | Xs])) :-
    !.
put(E, queue(List-[E | NewTail]), queue(List-NewTail)).

get(stack([X | Xs]), X, stack(Xs)) :-
    !.
get(queue([X | Xs]-Tail), X, queue(Xs-Tail)).

addall(E, Goal, stack(Xs), stack(Ys)) :-
    findall(E, Goal, Elements),
    reverse(Elements, ElementsRev),
    append(ElementsRev, Xs, Ys),
    !.
addall(E, Goal, queue(Xs-Elements), queue(Xs-NewTail)) :-
    findall(E, Goal, Elements, NewTail).

e(1,2).
e(1,3).
e(2,4).
e(2,5).
e(3,6).
e(3,7).


search(_, Collection, Acc, Acc) :-
    empty(Collection),
    !.
search(V1, Collection, Result, Acc) :-
    get(Collection, V, Rest),
    (member(V, Acc) ->
        search(V1, Rest, Result, Acc);
        (addall(U, e(V, U), Rest, RestWithNeighbours),
         search(V, RestWithNeighbours, Result, [V | Acc]))).

bfs(V1, Vs) :-
    empty(queue(X)),
    put(V1, queue(X), Q),
    search(V1, Q, RVs, []),
    reverse(RVs, Vs).
dfs(V1, Vs) :-
    empty(stack(X)),
    put(V1, stack(X), S),
    search(V1, S, RVs, []),
    reverse(RVs, Vs).

