:- [parser].
:- [eval].

run_file(FileName, MemOut) :-
    parse_file(FileName, Program),
    empty_mem(MemIn),
    eval_prog(Program, MemIn, MemOut).
