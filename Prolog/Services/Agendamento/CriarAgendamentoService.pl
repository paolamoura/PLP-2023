:- module(criarAgendamentoService, [criarAgendamento/7, deletarAgendamento/5]).
:- use_module('../../Models/Evento/ModelEvento.pl').
:- use_module('../../Models/Agendamento/Agendamento.pl').
:- use_module('../../Models/Usuario/ModelUsuario.pl').
:- use_module('../../Repository/agendamentoRepository.pl').
:- use_module('../../Repository/eventoRepository.pl').

agendamentoJaExiste(IdLocal,Data,Horario) :-
    getByAgendamento(IdLocal,Data, Horario, Rows),
    Rows \= [].

criarAgendamento(Nome, IdInstituicao, IdLocal, Matricula, Data, Horario, Evento) :-
    (agendamentoJaExiste(IdLocal, Data, Horario) ->
        % Retornar uma lista chamada ListaEspera
    ;
        saveAgendamento([IdLocal, Data, Horario, Matricula, ListaEspera]),
        agendar_compromisso(IdLocal, Data, Horario, Matricula)
        ).

deletarAgendamento(IdEvento, IdLocal, Matricula, Data, Horario) :-
    deleteEventoById(IdEvento),
    desalocar(IdLocal, Matricula, Data, Horario).