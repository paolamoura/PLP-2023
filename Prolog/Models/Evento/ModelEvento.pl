:- module(modelEvento, [createEvento/6]).
:- use_module('../../Utils/conversors.pl').


createEvento(Nome, IdInstituicao, IdLocal, Data, Horario, Evento) :-
    Evento = [Nome, IdInstituicao, IdLocal, Data, Horario].