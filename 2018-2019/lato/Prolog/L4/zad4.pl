solve(N, Qs) :-
    findall(X, between(1, N, X), Ns),
    solve(Ns, Qs, []).

solve([], Qs, Qs) :-
    !.
solve(Ns, Qs, Acc) :-
    select(N, Ns, NsRem),
    safe(Acc, N, 1),
    solve(NsRem, Qs, [N | Acc]).

safe([], _, _) :-
    !.
safe([Q | Qs], New, X) :-
    abs(Q - New) =\= X,
    NextX is X + 1,
    safe(Qs, New, NextX).
