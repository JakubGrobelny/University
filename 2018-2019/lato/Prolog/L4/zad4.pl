n_numbers(0, []) :-
    !.
n_numbers(N, [N | Ns]) :-
    M is N - 1,
    n_numbers(M, Ns).

solve(N, Qs) :-
    n_numbers(N, Ns),
    solve(Ns, Qs, []).

solve([], Qs, Qs) :-
    !.
solve(Ns, Qs, Acc) :-
    select(N, Ns, NsRem),
    safe(N, Acc, 1),
    solve(NsRem, Qs, [N | Acc]).

safe(_, [], _) :-
    !.
safe(New, [Q | Qs], X) :-
    New =\= Q,
    Diag is abs(Q - New),
    Diag =\= X,
    NextX is X + 1,
    safe(New, Qs, NextX).
