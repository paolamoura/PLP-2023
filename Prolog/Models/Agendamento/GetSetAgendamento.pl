:- module(getSetAgendamento, [getAgendamentosByMatricula/2]).

:- use_module('../../Repository/agendamentoRepository.pl').
:- use_module('../../Data/data.pl').
:- use_module('../../Utils/conversors.pl').

getAgendamentosByMatricula(Matricula, Agendamentos) :-
    getAgendamentosByMatriculaRep(Matricula, Agendamentos).

