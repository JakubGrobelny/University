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


evalExpr(int(N), _, N).
evalExpr(id(X), Mem, Val) :-
    mem_get(X, Mem, Val).
evalExpr(plus(L, R), Mem, Val) :-
    evalExpr(L, Mem, VL),
    evalExpr(R, Mem, VR),
    Val is VL + VR.
evalExpr(minus(L, R), Mem, Val) :-
    evalExpr(L, Mem, VL),
    evalExpr(R, Mem, VR),
    Val is VL - VR.
evalExpr(mult(L, R), Mem, Val) :-
    evalExpr(L, Mem, VL),
    evalExpr(R, Mem, VR),
    Val is VL * VR.
evalExpr(pow(B, P), Mem, Val) :-
    evalExpr(B, Mem, VB),
    evalExpr(P, Mem, VP),
    Val is VB ** VP.
evalExpr(div(L, R), Mem, Val) :-
    evalExpr(L, Mem, VL),
    evalExpr(R, Mem, VR),
    Val is VL div VR.
evalExpr(modulo(L, R), Mem, Val) :-
    evalExpr(L, Mem, VL),
    evalExpr(R, Mem, VR),
    Val is VL mod VR.


negated(true, false).
negated(false, true).

evalLog(not(Boolean), Mem, Val) :-
    evalLog(Boolean, Mem, true) ->
        Val = false;
        Val = true.

evalLog(and(P, Q), Mem, Val) :-
    evalLog(P, Mem, true) ->
        evalLog(Q, Mem, Val);
        Val = false.

evalLog(or(P, Q), Mem, Val) :-
    evalLog(P, Mem, false) ->
        evalLog(Q, Mem, Val);
        Val = true.

evalLog(eq(A, B), Mem, true) :-
    evalExpr(A, Mem, V),
    evalExpr(B, Mem, V),
    !.
evalLog(eq(_, _), _, false).

evalLog(not_eq(A, B), Mem, Val) :-
    evalLog(eq(A, B), Mem, NVal),
    negated(NVal, Val).

evalLog(less(L, R), Mem, true) :-
    evalExpr(L, Mem, LV),
    evalExpr(R, Mem, RV),
    LV < RV,
    !.
evalLog(less(_, _), _, false).

evalLog(greater(L, R), Mem, true) :-
    evalExpr(L, Mem, LV),
    evalExpr(R, Mem, RV),
    LV > RV,
    !.
evalLog(greater(_, _), _, false).

evalLog(greater_eq(L, R), Mem, Val) :-
    evalLog(less(L, R), Mem, NVal),
    negated(NVal, Val).

evalLog(less_eq(L, R), Mem, Val) :-
    evalLog(greater(L, R), Mem, NVal),
    negated(NVal, Val).

evalProg([], Mem, Mem).

evalProg([Instr | Prog], MemIn, MemOut) :-
    evalProg(Instr, MemIn, MemOut0),
    evalProg(Prog, MemOut0, MemOut).

evalProg(assign(Var, Expr), MemIn, MemOut) :-
    evalExpr(Expr, MemIn, Val),
    mem_set(MemIn, Var, Val, MemOut).

evalProg(if(Cond, Cons, Alt), MemIn, MemOut) :-
    evalLog(Cond, MemIn, true) ->
        evalProg(Cons, MemIn, MemOut);
        evalProg(Alt, MemIn, MemOut).

evalProg(while(Cond, Program), MemIn, MemOut) :-
    evalLog(Cond, MemIn, true),
    !,
    evalProg(Program, MemIn, MemOut0),
    evalProg(while(Cond, Program), MemOut0, MemOut).
evalProg(while(_, _), Mem, Mem).

