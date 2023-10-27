:- module(criarAgendamentoService, [criarAgendamento/4, deletarAgendamento/4]).
:- use_module('../../Models/Evento/ModelEvento.pl').
:- use_module('../../Models/Agendamento/Agendamento.pl').
:- use_module('../../Models/Usuario/ModelUsuario.pl').
:- use_module('../../Repository/eventoRepository.pl').
:- use_module('../../Repository/agendamentoRepository.pl').

criarAgendamento(Matricula, IdLocal, Data, Horario) :-
    string_para_atomo(Data, DataAtomo),
    string_para_atomo(Horario, HorarioAtomo),
    string_para_atomo(Matricula, MatriculaAtomo),
    agendar_compromisso(IdLocal, DataAtomo, HorarioAtomo, MatriculaAtomo).

deletarAgendamento(Matricula, IdLocal, Data, Horario) :-
    string_para_atomo(Data, DataAtomo),
    string_para_atomo(Horario, HorarioAtomo),
    string_para_atomo(Matricula, MatriculaAtomo),
    desalocar(IdLocal, DataAtomo, HorarioAtomo, MatriculaAtomo).

string_para_atomo(String, Atomo) :-
    atom_string(Atomo, String).