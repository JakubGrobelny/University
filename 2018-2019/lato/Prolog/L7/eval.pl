empty_mem([]).

mem_get(_, [], 0) :-
    !.
mem_get(Var, [(Var, Val) | _], Val) :-
    !.
mem_get(Var, [_ | Mem], Val) :-
    mem_get(Var, Mem, Val).

mem_set([], Var, Val, [(Var, Val)]) :-
    !.
mem_set([(Var, _) | Tail], Var, Val, [(Var, Val) | Tail]) :-
    !.
mem_set([(Var0, Val0) | Tail], Var, Val, [(Var, Val), (Var0, Val0) | Tail]) :-
    Var0 @> Var,
    !.
mem_set([Binding | Tail], Var, Val, [Binding | NewTail]) :-
    mem_set(Tail, Var, Val, NewTail).


eval_expr(int(N), _, N).
eval_expr(id(X), Mem, Val) :-
    mem_get(X, Mem, Val).
eval_expr(plus(L, R), Mem, Val) :-
    eval_expr(L, Mem, VL),
    eval_expr(R, Mem, VR),
    Val is VL + VR.
eval_expr(minus(L, R), Mem, Val) :-
    eval_expr(L, Mem, VL),
    eval_expr(R, Mem, VR),
    Val is VL - VR.
eval_expr(mult(L, R), Mem, Val) :-
    eval_expr(L, Mem, VL),
    eval_expr(R, Mem, VR),
    Val is VL * VR.
eval_expr(pow(B, P), Mem, Val) :-
    eval_expr(B, Mem, VB),
    eval_expr(P, Mem, VP),
    Val is VB ** VP.
eval_expr(div(L, R), Mem, Val) :-
    eval_expr(L, Mem, VL),
    eval_expr(R, Mem, VR),
    Val is VL div VR.
eval_expr(modulo(L, R), Mem, Val) :-
    eval_expr(L, Mem, VL),
    eval_expr(R, Mem, VR),
    Val is VL mod VR.


negated(true, false).
negated(false, true).

eval_log(not(Boolean), Mem, Val) :-
    eval_log(Boolean, Mem, true) ->
        Val = false;
        Val = true.

eval_log(and(P, Q), Mem, Val) :-
    eval_log(P, Mem, true) ->
        eval_log(Q, Mem, Val);
        Val = false.

eval_log(or(P, Q), Mem, Val) :-
    eval_log(P, Mem, false) ->
        eval_log(Q, Mem, Val);
        Val = true.

eval_log(eq(A, B), Mem, true) :-
    eval_expr(A, Mem, V),
    eval_expr(B, Mem, V),
    !.
eval_log(eq(_, _), _, false).

eval_log(not_eq(A, B), Mem, Val) :-
    eval_log(eq(A, B), Mem, NVal),
    negated(NVal, Val).

eval_log(less(L, R), Mem, true) :-
    eval_expr(L, Mem, LV),
    eval_expr(R, Mem, RV),
    LV < RV,
    !.
eval_log(less(_, _), _, false).

eval_log(greater(L, R), Mem, true) :-
    eval_expr(L, Mem, LV),
    eval_expr(R, Mem, RV),
    LV > RV,
    !.
eval_log(greater(_, _), _, false).

eval_log(greater_eq(L, R), Mem, Val) :-
    eval_log(less(L, R), Mem, NVal),
    negated(NVal, Val).

eval_log(less_eq(L, R), Mem, Val) :-
    eval_log(greater(L, R), Mem, NVal),
    negated(NVal, Val).

eval_prog([], Mem, Mem).

eval_prog([Instr | Prog], MemIn, MemOut) :-
    eval_prog(Instr, MemIn, MemOut0),
    eval_prog(Prog, MemOut0, MemOut).

eval_prog(assign(Var, Expr), MemIn, MemOut) :-
    eval_expr(Expr, MemIn, Val),
    mem_set(MemIn, Var, Val, MemOut).

eval_prog(if(Cond, Cons, Alt), MemIn, MemOut) :-
    eval_log(Cond, MemIn, true) ->
        eval_prog(Cons, MemIn, MemOut);
        eval_prog(Alt, MemIn, MemOut).

eval_prog(while(Cond, Program), MemIn, MemOut) :-
    eval_log(Cond, MemIn, true),
    !,
    eval_prog(Program, MemIn, MemOut0),
    eval_prog(while(Cond, Program), MemOut0, MemOut).
eval_prog(while(_, _), Mem, Mem).

