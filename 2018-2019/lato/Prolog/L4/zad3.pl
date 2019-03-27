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
    append(PartialResults, [FinalResult], Input),
    !.

gen_number([], []) :- 
    !.
gen_number([s | Xs], [N | Ns]) :-
    even(N),
    gen_number(Xs, Ns).
gen_number([c | Xs], [N | Ns]) :-
    odd(N),
    gen_number(Xs, Ns).

generate_partial_results(_, [], [], []) :-
    !.
generate_partial_results(Num, [Digit | Digits], [Template | Templates], [Result | Results]) :-
    ResultVal is Num * Digit,
    decimal(ResultVal, Result),
    gen_number(Template, Result),
    generate_partial_results(Num, Digits, Templates, Results).

sum([], 0, _) :-
    !.
sum([N | Ns], Sum, Offset) :-
    NextOffset is Offset * 10,
    sum(Ns, SumPrev, NextOffset),
    concat_number(N, NVal),
    Sum is SumPrev + (NVal * Offset).

solve(Input, Output) :-
    Input = [N1, N2 | Results],
    split_input(Results, PartialResults, FinalResult),
    gen_number(N1, Num1),
    gen_number(N2, Num2),
    concat_number(Num1, Num1Val),
    reverse(Num2, Num2Rev),
    generate_partial_results(Num1Val, Num2Rev, PartialResults, Generated),
    sum(Generated, Sum, 1),
    decimal(Sum, SumDigits),
    gen_number(FinalResult, SumDigits),
    append([Num1], [Num2], Out0),
    append(Out0, Generated, Out1),
    append(Out1, [SumDigits], Output).

