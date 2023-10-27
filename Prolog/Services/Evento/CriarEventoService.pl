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
        string_para_atomo(Data, DataAtomo),
        string_para_atomo(Horario, HorarioAtomo),
        string_para_atomo(Matricula, MatriculaAtomo),
        agendar_compromisso(IdLocal, DataAtomo, HorarioAtomo, MatriculaAtomo)
        ).

deletarEvento(IdEvento, IdLocal, Matricula, Data, Horario) :-
    atom_number(IdEvento, IdEventoNumber),
    deleteEventoById(IdEventoNumber),
    string_para_atomo(Data, DataAtomo),
    string_para_atomo(Horario, HorarioAtomo),
    string_para_atomo(Matricula, MatriculaAtomo),
    desalocar(IdLocal, DataAtomo, HorarioAtomo, MatriculaAtomo).

string_para_atomo(String, Atomo) :-
    atom_string(Atomo, String).