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

whitespace(X) :-
    member(X, [' ', '\r', '\n', '\t']).
digit(X) :-
    member(X, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']).
special(X) :-
    member(X, ['+', '-', '*', '^', ':', '=', '<', '>', '(', ')', ';']).
letter(X) :-
    X @>= 'a',
    X @=< 'z',
    !.
letter(X) :-
    X @>= 'A',
    X @=< 'Z'.
valid(X) :-
    whitespace(X);
    digit(X);
    special(X);
    letter(X).
keyword(KW) :-
    member(KW, [if, then, else, fi, while, do, od, div, mod, or, and, not]).

digit(D)      --> [D],    { digit(D)       }.
whitespace    --> [WS],   { whitespace(WS) }.
valid         --> [Char], { valid(Char)    }.
letter(Char)  --> [Char], { letter(Char)   }.
special(Char) --> [Char], { special(Char)  }.


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


keyword(kwrd(KW)) --> alphanum(KW), { keyword(KW) }.


identifier(id(Id)) --> alphanum(Id), { \+ keyword(Id) }.


alphanum(AlNum) --> letter(L), alphanum_(Tail), !,
    { 
        atomic_list_concat([L, Tail], AlNum)
    }.

alphanum_(AlNum) --> letter(L), alphanum_(Tail), !,
    {
        atomic_list_concat([L, Tail], AlNum)
    }.
alphanum_(AlNum) --> digit(L), alphanum_(Tail), !,
    { 
        atomic_list_concat([L, Tail], AlNum)
    }.
alphanum_('') --> [].


whatever --> valid, whatever.
whatever --> valid.

valid_sequence([]) --> [].
valid_sequence([V | Vs]) --> valid(V), valid_sequence(Vs), { \+ whitespace(V) }.

token([]) --> [].
token(Tokens) --> whitespace, !, token(Tokens).
token([KW| Tokens])  --> keyword(KW), !, token(Tokens).
token([Int| Tokens]) --> integer_literal(Int), !, token(Tokens).
token([Id| Tokens])  --> identifier(Id), !, token(Tokens).
% TODO: 1abc nie jest poprawnym tokenem
% TODO: wszystko
