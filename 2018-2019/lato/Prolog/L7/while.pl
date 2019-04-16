:- [parser].
:- [eval].

run_file(FileName, MemOut) :-
    parse_file(FileName, Program),
    empty_mem(MemIn),
    evalProg(Program, MemIn, MemOut).
