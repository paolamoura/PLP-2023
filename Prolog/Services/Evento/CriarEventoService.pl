:- module(criarEventoService, [criarEvento/7, deletarEvento/5]).
:- use_module('../../Models/Evento/ModelEvento.pl').
:- use_module('../../Models/Agendamento/Agendamento.pl').
:- use_module('../../Models/Usuario/ModelUsuario.pl').
:- use_module('../../Repository/eventoRepository.pl').

agendamentoJaExiste(IdLocal,Data,Horario) :-
    getByAgendamento(IdLocal,Data, Horario, Rows),
    Rows \= [].

criarEvento(Nome, IdInstituicao, IdLocal, Matricula, Data, Horario, Evento) :-
    (agendamentoJaExiste(IdLocal, Data, Horario) ->
        writeln('Erro: Horário já está ocupado!')
    ;

        createEvento(Nome, IdInstituicao, IdLocal, Data, Horario, Evento),
        saveEvento(Evento),
        agendar_compromisso(IdLocal, Data, Horario, Matricula)
        ).

deletarEvento(IdEvento, IdLocal, Matricula, Data, Horario) :-
    deleteEventoById(IdEvento),
    desalocar(IdLocal, Matricula, Data, Horario).