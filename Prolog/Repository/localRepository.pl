:- module(localRepository, [saveLocal/1, deleteById/1, getById/2, getAllLocal/1]).
:- use_module("../Data/data.pl").
:- use_module("../Utils/conversors.pl").

% Fato dinâmico para gerar o id dos agentes
id(0).
incrementa_id :- retract(id(X)), Y is X + 1, assert(id(Y)).
:- dynamic id/1.

path('local.csv').

saveLocal(Local) :- 
    path(Path),
    id(ID), incrementa_id,
    insertAndConvert(ID, Local, Row),
    data:save(Path, Row).

deleteById(Id) :- path(Path), data:delete(Path, Id).

getById(Id, Local) :- path(Path), data:getById(Path, Id, Row), listToRow(Local, Row).

getAllLocal(Locais) :- path(Path), data:getAllRows(Path, Rows), rowsToLists(Rows, Locais).