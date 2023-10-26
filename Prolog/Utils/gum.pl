:- module(gum, [choose/2, input/2, password/1]).

:- use_module(library(process)).
:- use_module('./parsers.pl').

choose([], '0').
choose(Options,Choosen) :-
    run(gum, [choose|Options], RawOutput),
    remove_nl(RawOutput, Choosen).

input(Options, Answer) :-
    run(gum, [input|Options], Output),
    remove_nl(Output, Answer).

password(Answer) :-
    input(['--password'], Answer).

run(Command, Args, Output) :-
    process_create(path(Command), Args, [stdout(pipe(Out))]),
    read_string(Out, _, Output).