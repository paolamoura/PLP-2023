:- module(modelEvento, [createEvento/9]).
:- use_module('../Utils/conversors.pl').


createEvento(Nome, IdInstituicao, IdLocal, IdAgendamento, Inscritos, Capacidade, Vagas, Evento) :-
    Evento = evento(Nome, IdInstituicao, IdLocal, IdAgendamento, Inscritos, Capacidade, Vagas, 20, 3).