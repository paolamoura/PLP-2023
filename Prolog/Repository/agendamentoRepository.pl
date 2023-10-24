:- module(agendamentoRepository, [saveAgendamento/1, deleteById/1, getById/2, getAllAgendamento/1]).
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
    insertAndConvert(ID, Agendamento, Row),
    data:save(Path, Row).

deleteById(Id) :- path(Path), data:delete(Path, Id).

getById(Id, Agendamento) :- path(Path), data:getById(Path, Id, Row), parseRow(Row, Agendamento).

getAllAgendamento(Agendamentos) :- path(Path), data:getAllRows(Path, Rows), parseTable(Rows, Agendamentos).

updateAgendamento(Id, Agendamento) :- path(Path), listToRow(Agendamento, Row), data:update(Path, Id, Row).