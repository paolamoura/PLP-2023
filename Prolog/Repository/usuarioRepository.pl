:- module(usuarioRepository, [saveUsuario/1, deleteById/1, getById/2, getAllUsuario/1, getByMatricula/2]).
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
    insertAtFirst(ID, Usuario, List),
    parseList(List, Row),
    saveRow(Path, Row).

getByMatricula(Matricula, Usuario) :- path(Path), atom_number(Matricula, MatriculaInt), getByMatriculaRow(Path, MatriculaInt, Row), parseRow(Row, Usuario).

deleteById(Id) :- path(Path), deleteRow(Path, Id).

getById(Id, Usuario) :- path(Path), getByIdRow(Path, Id, Row), parseRow(Row, Usuario).

getAllUsuario(Usuarios) :- path(Path), getAllRows(Path, Rows), parseTable(Rows, Usuarios).

updateUsuario(Id, Usuario) :- path(Path), listToRow(Usuario, Row), updateRow(Path, Id, Row). 