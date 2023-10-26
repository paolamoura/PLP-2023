:- module(modelEvento, [createEvento/8]).
:- use_module('../../Utils/conversors.pl').


createEvento(Nome, IdInstituicao, IdLocal, IdAgendamento, Inscritos, Capacidade, Vagas, Evento) :-
    Evento = [Nome, IdInstituicao, IdLocal, IdAgendamento, Inscritos, Capacidade, Vagas].