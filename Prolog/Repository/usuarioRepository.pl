:- module(usuarioRepository, [saveUsuario/1, deleteUsuarioById/1, getUsuarioById/2, getAllUsuario/1, getByMatricula/2, updateUsuario/2]).

:- use_module("../Data/data.pl").
:- use_module("../Utils/conversors.pl").
:- use_module("../Utils/parsers.pl").

:- dynamic id/1.

path('usuarios.csv').

% Fato estático para inicializar o ID ao carregar o módulo
:- initialization(loadId).

loadId :-
    path(Path),
    getLastRow(Path, LastRow),
    parseRow(LastRow, LastList),
    (LastList = ['id', 'matricula', 'nome', 'senha'] ->
        assertz(id(0))
    ;
        primeiro_elemento(LastList, LastId),
        assertz(id(LastId))
    ).

primeiro_elemento([Primeiro|_], Primeiro).

saveUsuario(Usuario) :-
    path(Path),
    id(ID),
    NovoID is ID + 1,
    insertAtFirst(NovoID, Usuario, List),
    parseList(List, Row),
    saveRow(Path, Row),
    retractall(id(_)),
    assertz(id(NovoID)).

getByMatricula(Matricula, Usuario) :- path(Path), atom_number(Matricula, MatriculaInt), getByMatriculaRow(Path, MatriculaInt, Row), parseRow(Row, Usuario).

deleteUsuarioById(Id) :- path(Path), deleteRow(Path, Id).

getUsuarioById(Id, Usuario) :- path(Path), getByIdRow(Path, Id, Row), parseRow(Row, Usuario).

getAllUsuario(Usuarios) :- path(Path), getAllRows(Path, Rows), parseTable(Rows, Usuarios).

updateUsuario(Id, Usuario) :- path(Path), listToRow(Usuario, Row), updateRow(Path, Id, Row).