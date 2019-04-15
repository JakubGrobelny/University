:- dynamic addend/3.
char_codes_to_atoms([], []) :-
    !.
char_codes_to_atoms([Char | Chars], [Atom | Atoms]) :-
    char_code(Atom, Char),
    char_codes_to_atoms(Chars, Atoms).

file_to_list(FileName, List) :-
    open(FileName, read, Stream),
    read_string(Stream, "", "", _, String),
    string_to_list(String, CodesList),
    char_codes_to_atoms(CodesList, List),
    close(Stream).

tokenize_file(FileName, Tokens) :-
    file_to_list(FileName, FileContents),
    phrase(token(Tokens), FileContents).

parse_tokens(Tokens, Program) :-
    phrase(program(Program), Tokens).

parse_file(FileName, Program) :-
    tokenize_file(FileName, Tokens),
    parse_tokens(Tokens, Program).


is_letter(X) :-
    X @>= 'a',
    X @=< 'z',
    !.
is_letter(X) :-
    X @>= 'A',
    X @=< 'Z'.

whitespace(' ')  --> [' '], !.
whitespace('\r') --> ['\r'], !.
whitespace('\n') --> ['\n'], !.
whitespace('\t') --> ['\t'], !.

digit('0') --> ['0'], !.
digit('1') --> ['1'], !.
digit('2') --> ['2'], !.
digit('3') --> ['3'], !.
digit('4') --> ['4'], !.
digit('5') --> ['5'], !.
digit('6') --> ['6'], !.
digit('7') --> ['7'], !.
digit('8') --> ['8'], !.
digit('9') --> ['9'], !.

special('+') --> ['+'].
special('-') --> ['-'].
special('*') --> ['*'].
special('^') --> ['^'].
special(':') --> [':'].
special('=') --> ['='].
special('<') --> ['<'].
special('>') --> ['>'].
special('(') --> ['('].
special(')') --> [')'].
special(';') --> [';'].

letter(Char)  --> [Char], { is_letter(Char) }.

valid(Char) --> digit(Char); letter(Char); special(Char).

is_keyword(Atom) :-
    member(Atom, [if, then, else, fi, while, do, od, div, mod, or, and, not]).

keyword(kwrd(Kw)) --> alphanum(AlNum),
    {
        atomic_list_concat(AlNum, Kw),
        is_keyword(Kw)
    }.

identifier(id(Id)) --> alphanum(AlNum),
    {
        atomic_list_concat(AlNum, Id),
        \+ is_keyword(Id)
    }.


integer_literal(int(N)) --> digit(D), integer_literal_(Digits), !,
    {
        atomic_list_concat([D, Digits], Number),
        atom_number(Number, N)
    }.
integer_literal(int(N)) --> digit(D), { atom_number(D, N) }.

integer_literal_(Digits) --> digit(D), integer_literal_(DTail), !,
    {
        atomic_list_concat([D, DTail], Digits)
    }.
integer_literal_(D) --> digit(D).

alphanum([L | Tail]) --> letter(L), alphanum_(Tail).

alphanum_([L | Tail]) --> letter(L), !, alphanum_(Tail).
alphanum_([L | Tail]) --> digit(L), !, alphanum_(Tail).
alphanum_([]) --> [].

comment_tail --> [_], comment_tail.
comment_tail --> ['*', ')'].
comment --> ['(', '*'], comment_tail.

operator(op(less_eq))    --> ['<', '='], !.
operator(op(greater_eq)) --> ['>', '='], !.
operator(op(not_eq))     --> ['<', '>'], !.
operator(op(assign))     --> [':', '='], !.
operator(op(plus))       --> ['+'], !.
operator(op(minus))      --> ['-'], !.
operator(op(mult))       --> ['*'], !.
operator(op(pow))        --> ['^'], !.
operator(op(eq))         --> ['='], !.
operator(op(less))       --> ['<'], !.
operator(op(greater))    --> ['>'], !.

token(Tokens)                --> whitespace(_), !, token(Tokens).
token(Tokens)                --> comment, !, token(Tokens).
token([Int| Tokens])         --> integer_literal(Int), !, token(Tokens).
token([Kw| Tokens])          --> keyword(Kw), !, token(Tokens).
token([Id | Tokens])         --> identifier(Id), !, token(Tokens).
token([Op | Tokens])         --> operator(Op), !, token(Tokens).
token([semicolon | Tokens])  --> [';'], !, token(Tokens).
token([leftparen | Tokens])  --> ['('], !, token(Tokens).
token([rightparen | Tokens]) --> [')'], !, token(Tokens).
token([])                    --> [].


program(Program) --> instruction(Instr), program__([Instr], Program).
program__(Acc, Res) --> 
    [semicolon], !, instruction(I1), program__([I1 | Acc], Res).
program__(Acc, Res) --> [], { reverse(Acc, Res) }.

instruction(assignement(Var, Expr)) --> [id(Var)], !, [op(assign)], arith(Expr).
instruction(if(Cond, Cons, Alt)) --> 
    [kwrd(if)], logical(Cond), 
    [kwrd(then)], program(Cons), 
    [kwrd(else)], !, program(Alt), 
    [kwrd(fi)].
instruction(if(Cond, Cons, [])) --> 
    [kwrd(if)], !, logical(Cond), 
    [kwrd(then)], program(Cons), 
    [kwrd(fi)].
instruction(while(Cond, Program)) -->
    [kwrd(while)], logical(Cond), [kwrd(do)],
    program(Program),
    [kwrd(od)].

logical(Expr) --> logical_addend(X1), logical__(X1, Expr).
logical__(Acc, Res) --> 
    [op(or)], !, logical_addend(X1), logical__(or(Acc, X1), Res).
logical__(Acc, Acc) --> [].

logical_addend(Expr) --> logical_factor(X1), logical_addend__(X1, Expr).
logical_addend__(Acc, Res) --> 
    [kwrd(and)], !, logical_factor(X1), logical_addend__(and(Acc, X1), Res).
logical_addend__(Acc, Acc) --> [].

% logical_addend(and(Lhs, Rhs)) --> 
%     logical_addend(Lhs), [kwrd(and)], !, logical_factor(Rhs).
% logical_addend(Addend) --> logical_factor(Addend).

logical_factor(not(Boolean)) --> [lwrd(not)], !, logical_factor(Boolean).
logical_factor(Factor) --> relational(Factor).

relational(Comparison) --> arith(Lhs), relop(Op), arith(Rhs),
    {
        Comparison =.. [Op, Lhs, Rhs]
    }.

arith(ArithExpr) --> addend(X1), arith__(X1, ArithExpr).
arith__(Acc, Res) --> additive(Op), !, addend(X1), arith__(Expr, Res),
    {
        Expr =.. [Op, Acc, X1]
    }.
arith__(A, A) --> [].

addend(ArithExpr) --> factor(X1), addend__(X1, ArithExpr).
addend__(Acc, Res) --> multiplicative(Op), !, factor(X1), addend__(Expr, Res),
    {
        Expr =.. [Op, Acc, X1]
    }.
addend__(A, A) --> [].

factor(pow(Base, Pow)) --> atomic_expr(Base), [op(pow)], !, factor(Pow).
factor(Expr) --> atomic_expr(Expr).

atomic_expr(ArithExpr) --> [leftparen], arith(ArithExpr), [rightparen].
atomic_expr(int(Integer)) --> [int(Integer)].
atomic_expr(id(Identifier)) --> [id(Identifier)].

relop(Operator) --> [op(Operator)].

additive(plus) --> [op(plus)].
additive(minus) --> [op(minus)].

multiplicative(mult) --> [op(mult)].
multiplicative(div) --> [kwrd(div)].
multiplicative(mod) --> [kwrd(mod)].


