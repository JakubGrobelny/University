empty(stack([])).
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
    findall(E, Goal, Elements, [NewTail]).




