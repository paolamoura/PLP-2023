:- module(CriarEventoService, [criarEvento/12]).
:- use_module('../../Models/Evento/ModelEvento.pl').
:- use_module('../../Models/Agendamento/Agendamento.pl').
:- use_module('../../Models/Usuario/ModelUsuario.pl').
:- use_module('../../Repository/eventoRepository.pl').

criarEvento(Nome, IdInstituicao, IdLocal, IdAgendamento, Inscritos, Capacidade, Vagas, Matricula, NomeLocal, Data, Horario, Evento) :-
    createEvento(Nome, IdInstituicao, IdLocal, IdAgendamento, Inscritos, Capacidade, Vagas, Evento),
    saveEvento(Evento),
    agendar_compromisso(IdLocal, Data, Horario, IdInstituicao).