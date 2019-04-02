sum_vars(X, Y, Z) :-
    vars(X, XVars),
    vars(Y, YVars),
    append(XVars, YVars, Z0),
    sort(Z0, Z).

vars(X, []) :-
    (X = e;
     X = pi;
     number(X)),
    !.
vars(X, [X]) :-
    atom(X),
    !.
vars(X+Y, Z) :-
    sum_vars(X, Y, Z).
vars(X-Y, Z) :-
    sum_vars(X, Y, Z).
vars(X*Y, Z) :-
    sum_vars(X, Y, Z).
vars(X/Y, Z) :-
    sum_vars(X, Y, Z).
vars(X**Y, Z) :-
    sum_vars(X, Y, Z).
vars(sqrt(X), Y) :-
    vars(X, Y).
vars(sin(X), Y) :-
    vars(X, Y).
vars(cos(X), Y) :-
    vars(X, Y).
vars(tan(X), Y) :-
    vars(X, Y).
vars(ctg(X), Y) :-
    vars(X, Y).
vars(log(X), Y) :-
    vars(X, Y).
vars(log10(X), Y) :-
    vars(X, Y).
vars(exp(X), Y) :-
    vars(X, Y).

eval(pi, _, Val) :-
    Val is pi,
    !.
eval(e, _, Val) :-
    Val is e,
    !.
eval(Num, _, Num) :-
    number(Num),
    !.
eval(X, [[X, Val] | _], Val) :-
    atom(X),
    !.
eval(X, [_ | Env], Val) :-
    atom(X),
    eval(X, Env, Val),
    !.
eval(X+Y, Env, Val) :-
    eval(X, Env, ValL),
    eval(Y, Env, ValR),
    Val is ValL + ValR.
eval(X*Y, Env, Val) :-
    eval(X, Env, ValL),
    eval(Y, Env, ValR),
    Val is ValL * ValR.
eval(X/Y, Env, Val) :-
    eval(X, Env, ValL),
    eval(Y, Env, ValR),
    Val is ValL / ValR.
eval(X-Y, Env, Val) :-
    eval(X, Env, ValL),
    eval(Y, Env, ValR),
    Val is ValL - ValR.
eval(X**Y, Env, Val) :-
    eval(X, Env, ValL),
    eval(Y, Env, ValR),
    Val is ValL ** ValR.
eval(sqrt(X), Env, Val) :-
    eval(X, Env, ValX),
    Val is sqrt(ValX).
eval(sin(X), Env, Val) :-
    eval(X, Env, ValX),
    Val is sin(ValX).
eval(cos(X), Env, Val) :-
    eval(X, Env, ValX),
    Val is cos(ValX).
eval(tan(X), Env, Val) :-
    eval(X, Env, ValX),
    Val is tan(ValX).
eval(ctg(X), Env, Val) :-
    eval(X, Env, ValX),
    Val is cos(ValX) / sin(ValX).
eval(log(X), Env, Val) :-
    eval(X, Env, ValX),
    Val is log(ValX).
eval(log10(X), Env, Val) :-
    eval(X, Env, ValX),
    Val is log10(ValX).
eval(exp(X), Env, Val) :-
    eval(X, Env, ValX),
    Val is exp(ValX).


