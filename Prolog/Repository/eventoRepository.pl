:- module(eventoRepository, [saveEvento/1, deleteById/1, getById/2, getAllEvento/1]).
:- use_module("../Data/data.pl").
:- use_module("../Utils/conversors.pl").
:- use_module("../Utils/parsers.pl").

% Fato dinâmico para gerar o id dos agentes
id(0).
incrementa_id :- retract(id(X)), Y is X + 1, assert(id(Y)).
:- dynamic id/1.

path('eventos.csv').

saveEvento(Evento) :- 
    path(Path),
    id(ID), incrementa_id,
    insertAtFirst(ID, Evento, List),
    parseList(List, Row),
    saveRow(Path, Row).

deleteById(Id) :- path(Path), deleteRow(Path, Id).

getById(Id, Evento) :- path(Path), getByIdRow(Path, Id, Row), parseRow(Row, Evento).

getAllEvento(Eventos) :- path(Path), getAllRows(Path, Rows), parseTable(Rows, Eventos).

updateEvento(Id, Evento) :- path(Path), listToRow(Evento, Row), updateRow(Path, Id, Row).