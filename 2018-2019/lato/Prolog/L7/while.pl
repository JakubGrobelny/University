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

% keyword(kwrd(if))    --> ['i', 'f'].
% keyword(kwrd(then))  --> ['t', 'h', 'e', 'n'].
% keyword(kwrd(else))  --> ['e', 'l', 's', 'e'].
% keyword(kwrd(fi))    --> ['f', 'i'].
% keyword(kwrd(while)) --> ['w', 'h', 'i', 'l', 'e'].
% keyword(kwrd(do))    --> ['d', 'o'].
% keyword(kwrd(od))    --> ['o', 'd'].
% keyword(kwrd(div))   --> ['d', 'i', 'v'].
% keyword(kwrd(mod))   --> ['m', 'o', 'd'].
% keyword(kwrd(or))    --> ['o', 'r'].
% keyword(kwrd(and))   --> ['a', 'n', 'd'].
% keyword(kwrd(not))   --> ['n', 'o', 't'].

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

operator(less_eq)    --> ['<', '='], !.
operator(greater_eq) --> ['>', '='], !.
operator(not_eq)     --> ['<', '>'], !.
operator(assign)     --> [':', '='], !.
operator(plus)       --> ['+'], !.
operator(minus)      --> ['-'], !.
operator(mult)       --> ['*'], !.
operator(pow)        --> ['^'], !.
operator(eq)         --> ['='], !.
operator(less)       --> ['<'], !.
operator(greater)    --> ['>'], !.
operator(semicolon)  --> [';'], !.

token(Tokens)                --> whitespace(_), !, token(Tokens).
token(Tokens)                --> comment, !, token(Tokens).
token([Int| Tokens])         --> integer_literal(Int), !, token(Tokens).
token([Kw| Tokens])          --> keyword(Kw), !, token(Tokens).
token([Id | Tokens])         --> identifier(Id), !, token(Tokens).
token([Op | Tokens])         --> operator(Op), !, token(Tokens).
token([lefparen | Tokens])   --> ['('], !, token(Tokens).
token([rightparen | Tokens]) --> [')'], !, token(Tokens).
token([])                    --> [].


program([Instr | Program]) --> instruction(Instr), !, program(Program).
program([]) --> [].

instruction(assignement(Var, Expr)) --> [id(Var)], !, [assign], arith(Expr).
instruction(if_statement(Cond, Cons, Alt)) --> 
    [kwrd(if)], logical(Cond), 
    [kwrd(then)], program(Cons), 
    [kwrd(else)], !, program(Alt), 
    [kwrd(fi)].
instruction(if_statement(Cond, Cons, [])) --> 
    [kwrd(if)], !, logical(Cond), 
    [kwrd(then)], program(Cons), 
    [kwrd(fi)].









