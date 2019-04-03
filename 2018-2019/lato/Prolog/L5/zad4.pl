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

e(1, 2).
e(1, 3).
e(1, 4).
e(2, 5).
e(5, 7).
e(4, 6).
e(6, 7).
e(7, 3).

find_path_bfs(From, To, Result) :-
    empty(queue(X)),
    put((From, [From]), queue(X), Q),
    find_path(To, Q, RevRes),
    reverse(RevRes, Result).
find_path_dfs(From, To, Result) :-
    empty(stack(X)),
    put((From, [From]), stack(X), S),
    find_path(To, S, RevRes),
    reverse(RevRes, Result).

find_path(To, Collection, Result) :-
    \+ empty(Collection),
    get(Collection, (To, Result), _),
    !.
find_path(To, Collection, Result) :-
    \+ empty(Collection),
    get(Collection, (V, Path), Rest),
    addall((U, [U | Path]), (e(V, U), \+ member(U, Path)), Rest, Collection0),
    find_path(To, Collection0, Result).


search(Collection, Acc, Acc) :-
    empty(Collection),
    !.
search(Collection, Result, Acc) :-
    get(Collection, V, Rest),
    (member(V, Acc) ->
        search(Rest, Result, Acc);
        (addall(U, e(V, U), Rest, RestWithNeighbours),
         search(RestWithNeighbours, Result, [V | Acc]))).

bfs(V1, Vs) :-
    empty(queue(X)),
    put(V1, queue(X), Q),
    search(Q, RVs, []),
    reverse(RVs, Vs).
dfs(V1, Vs) :-
    empty(stack(X)),
    put(V1, stack(X), S),
    search(S, RVs, []),
    reverse(RVs, Vs).

