even([]).
even([_, _ | T]) :-
    even(T).

palindrome([]).
palindrome([_]).
palindrome([H | T]) :-
    last(H, T),
    init(T, X),
    palindrome(X).

singleton([_]).

head(H, [H | _]).

last([X], X).
last([_ | T], X) :-
    last(T, X).

tail(T, [_ | T]).

init([_], []).
init([H | T0], [H | T1]) :-
    init(T0, T1).

prefix([], _).
prefix([H | T0], [H | T1]) :-
    prefix(T0, T1).

suffix(X, X).
suffix(X, [_ | T]) :-
    suffix(X, T).