even(X) :-
    member(X, [0,2,4,6,8]).
odd(X) :-
    member(X, [1,3,5,7,9]).

concat_number(Digits, Number) :-
    concat_number(Digits, Number, _).
concat_number([N], N, 1) :-
    !.
concat_number([Digit | Digits], N, Pow) :-
    concat_number(Digits, M, PrevPow),
    Pow is 10 * PrevPow,
    DigitVal is Pow * Digit,
    N is DigitVal + M.

decimal(N, Digits) :-
    decimal(N, Digits, []).
decimal(N, [N | Acc], Acc) :-
    N < 10,
    !.
decimal(N, Digits, Acc) :-
    Digit is mod(N, 10),
    Div is N // 10,
    decimal(Div, Digits, [Digit | Acc]).

split_input(Input, PartialResults, FinalResult) :-
    length(FinalResult, 1),
    append(PartialResults, FinalResult, Input),
    !.

solve(Input) :-
    Input = [N1, N2, Results],
    split_input(Results, PartialResults, FinalResult),
