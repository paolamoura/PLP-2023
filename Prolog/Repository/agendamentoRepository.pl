:- module(agendamentoRepository, [saveAgendamento/1, deleteAgendamentoById/1, getAgendamentoById/2, getAllAgendamento/1, updateAgendamento/2, getAgendamentosByMatriculaRep/2]).

:- use_module("../Data/data.pl").
:- use_module("../Utils/conversors.pl").
:- use_module("../Utils/parsers.pl").

% Fato dinâmico para gerar o id dos agentes
id(0).
incrementa_id :- retract(id(X)), Y is X + 1, assert(id(Y)).
:- dynamic id/1.

path('agendamentos.csv').

saveAgendamento(Agendamento) :- 
    path(Path),
    id(ID), incrementa_id,
    insertAtFirst(ID, Agendamento, List),
    parseList(List, Row),
    saveRow(Path, Row).

deleteAgendamentoById(Id) :- path(Path), deleteRow(Path, Id).

getAgendamentoById(Id, Agendamento) :- path(Path), getByIdRow(Path, Id, Row), parseRow(Row, Agendamento).

getAgendamentosByMatriculaRep(Matricula, Agendamentos) :- path(Path), atom_number(Matricula, MatriculaInt), getAgendamentosByMatriculaRow(Path, MatriculaInt, Row), parseTable(Row, Agendamentos).

getAllAgendamento(Agendamentos) :- path(Path), getAllRows(Path, Rows), parseTable(Rows, Agendamentos).

updateAgendamento(Id, Agendamento) :- path(Path), listToRow(Agendamento, Row), updateRow(Path, Id, Row).