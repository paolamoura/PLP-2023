:- module(ModelLocal, [createLocal/3]).
:- use_module('../../Utils/conversors.pl').

createLocal(Nome, Materiais, Local) :-
    Local = [Nome, Materiais].