:- module(usuarioRepository, [saveUsuario/1, deleteById/1, getById/2, getAllUsuario/1]).
:- use_module("../Data/data.pl").
:- use_module("../Utils/conversors.pl").
:- use_module("../Utils/parsers.pl").

% Fato dinâmico para gerar o id dos agentes
id(0).
incrementa_id :- retract(id(X)), Y is X + 1, assert(id(Y)).
:- dynamic id/1.

path('usuarios.csv').

saveUsuario(Usuario) :-
    path(Path),
    id(ID), incrementa_id,
    insertAndConvert(ID, Usuario, Row),
    data:save(Path, Row).

deleteById(Id) :- path(Path), data:delete(Path, Id).

getById(Id, Usuario) :- path(Path), data:getById(Path, Id, Row), parseRow(Row, Usuario).

getAllUsuario(Usuarios) :- path(Path), data:getAllRows(Path, Rows), parseTable(Rows, Usuarios).

updateUsuario(Id, Usuario) :- path(Path), listToRow(Usuario, Row), data:update(Path, Id, Row). 