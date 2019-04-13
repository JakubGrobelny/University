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
    phrase(lexer(Tokens), FileContents).

digit_val(D, N) :-
    char_code(D, Val),
    char_code('0', Zero),
    N is Val - Zero.

whitespace(X) :-
    member(X, [' ', '\r', '\n', '\t']).
digit(X) :-
    member(X, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']).
special(X) :-
    member(X, ['+', '-', '*', '^', ':', '=', '<', '>', '(', ')', ';']).
letter(X) :-
    X @=< 'a',
    X @>= 'z',
    !.
letter(X) :-
    X @=< 'A',
    X @>= 'Z'.
valid(X) :-
    whitespace(X);
    digit(X);
    special(X);
    letter(X).

whitespace --> 

digit(N) --> ['0'].
digit(N) --> ['1'].
digit(N) --> ['2'].
digit(N) --> ['3'].
digit(N) --> ['4'].
digit(N) --> ['5'].
digit(N) --> ['6'].
digit(N) --> ['7'].
digit(N) --> ['8'].
digit(N) --> ['9'].


token([]) --> [].
token(Tokens) --> whitespace, !, token(Tokens).
token(Tokens) --> ['(', '*'], !, _, ['*', ')'], token(Tokens).
token([KW| Tokens]) --> keyword(KW), !, token(Tokens).

keyword(kwrd(if))    --> ['i', 'f'].
keyword(kwrd(then))  --> ['t', 'h', 'e', 'n'].
keyword(kwrd(else))  --> ['e', 'l', 's', 'e'].
keyword(kwrd(fi))    --> ['f', 'i'].
keyword(kwrd(while)) --> ['w', 'h', 'i', 'l', 'e'].
keyword(kwrd(do))    --> ['d', 'o'].
keyword(kwrd(od))    --> ['o', 'd'].
keyword(kwrd(div))   --> ['d', 'i', 'v'].
keyword(kwrd(mod))   --> ['m', 'o', 'd'].
keyword(kwrd(or))    --> ['o', 'r'].
keyword(kwrd(and))   --> ['a', 'n', 'd'].
keyword(kwrd(not))   --> ['n', 'o', 't'].
