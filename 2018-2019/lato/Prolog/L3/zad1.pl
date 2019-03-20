exp(_, 0, 1) :-
    !.
exp(Base, Exp, Res) :-
    ExpLess is Exp - 1,
    exp(Base, ExpLess, PrevRes),
    Res is Base * PrevRes.

exp(_, 0, _, 1) :-
    !.
exp(Base, Exp, Mod, Res) :-
    ExpLess is Exp - 1,
    exp(Base, ExpLess, Mod, PrevRes),
    Res is (PrevRes * Base) mod Mod.

factorial(0, 1) :-  
    !.
factorial(N, M) :-
    Prev is N - 1,
    factorial(Prev, PrevFact),
    M is N * PrevFact.

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

filter([], []).
filter([H | T], FT) :-
    H < 0,
    !,
    filter(T, FT).
filter([H | T], [H | FT]) :-
    filter(T, FT).