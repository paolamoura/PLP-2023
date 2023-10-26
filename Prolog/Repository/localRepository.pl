:- module(localRepository, [saveLocal/1, deleteLocalById/1, getLocalById/2, getAllLocal/1]).
:- use_module("../Data/data.pl").
:- use_module("../Utils/conversors.pl").
:- use_module("../Utils/parsers.pl").

% Fato dinâmico para gerar o id dos agentes
id(0).
incrementa_id :- retract(id(X)), Y is X + 1, assert(id(Y)).
:- dynamic id/1.

path('local.csv').

saveLocal(Local) :- 
    path(Path),
    id(ID), incrementa_id,
    insertAtFirst(ID, Local, List),
    parseList(List, Row),
    saveRow(Path, Row).

deleteLocalById(Id) :- path(Path), deleteRow(Path, Id).

getLocalById(Id, Local) :- path(Path), getByIdRow(Path, Id, Row), parseRow(Row, Local). 

getAllLocal(Locais) :- path(Path), getAllRows(Path, Rows), parseTable(Rows, Locais). 

updateLocal(Id, Local) :- path(Path), listToRow(Local, Row), updateRow(Path, Id, Row). 