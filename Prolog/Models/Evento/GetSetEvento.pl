:- module(getSetEvento, [getEventoNome/2, getInstituicao/2, getLocal/2, getIdAgendamento/2, getInscritos/2, getCapacidade/2, getVagas/2]).

:- use_module('../../Repository/eventoRepository.pl').
:- use_module('../../Utils/conversors.pl').

getEventoNome(ID, Nome) :- 
    getEventoById(ID, Evento),
    listToEvent(Evento, Resultado),
    Resultado = evento(_, Nome, _, _, _, _, _, _).

getInstituicao(ID, IdInstituicao) :- 
    getEventoById(ID, Evento),
    listToEvent(Evento, Resultado),
    Resultado = evento(_, _, IdInstituicao, _, _, _, _, _).

getLocal(ID, Local) :- 
    getEventoById(ID, Evento),
    listToEvent(Evento, Resultado),
    Resultado = evento(_, _, _, IdLocal, _, _, _, _).

getIdAgendamento(ID, IdAgendamento) :- 
    getEventoById(ID, Evento),
    listToEvent(Evento, Resultado),
    Resultado = evento(_, _, _, _, DataEvento, _, _, _).

getInscritos(ID, Inscritos) :-
    getEventoById(ID, Evento),
    listToEvent(Evento, Resultado),
    Resultado = evento(_, _, _, _, _, Inscritos, _, _).

getCapacidade(ID, Capacidade) :-
    getEventoById(ID, Capacidade),
    listToEvent(Evento, Resultado),
    Resultado = evento(_, _, _, _, _, _, Capacidade, _).

getVagas(ID, Vagas) :-
    getEventoById(ID, Evento),
    listToEvent(Evento, Resultado),
    Resultado = evento(_, _, _, _, _, _, _, Vagas).
