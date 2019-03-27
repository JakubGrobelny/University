number(_, 0).
number(Max, N) :-
    number(Max, M),
    N is M + 1,
    N =< Max.

solve(0, []) :- !.
solve(N, [Q | Qs]) :-
    number(N, Q),
    solve(N, Qs),
    safe([Q | Qs]).

% solve(N, Qs) :-
%     numlist(1, N, Xs),
%     permutation(Qs, Xs),
%     safe(Qs).

% 0 hetmanów się nie zbija
safe([]).
% ustawienie jest OK jeżeli Qs się nie zbijają 
% i Q nie zbija się z żadnym z Qs
safe([Q | Qs]) :-
    safe(Qs, Q, 1),
    safe(Qs).

% 0 hetmanów nie zbija żadnego hetmana
safe([], _, _).
safe([Q | Qs], Queen, Diagonal) :-
    Queen \= Q, % są w różnych wierszach
    Abs is abs(Queen - Q),
    Abs \= Diagonal,
    NextDiagonal is Diagonal + 1,
    safe(Qs, Queen, NextDiagonal).



