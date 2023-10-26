:- module(data, [saveRow/2,
    deleteRow/2,
    getByIdRow/3,
    getAllRows/2,
    updateRow/3,
    getByMatriculaRow/3,
    getLastRow/2,
    getAgendamentosByMatriculaRow/3,
    getByIdAgendamentoRow/3
    ]).
:- use_module(library(csv)).
:- use_module("../Utils/conversors.pl").
:- use_module("../Utils/parsers.pl").

% Lendo o arquivo CSV
lerCSV(FilePath, Rows) :- 
    csv_read_file(FilePath, Rows).

% Salvar Linha no CSV.
saveRow(FilePath, Data) :-
    atom_concat('Data/', FilePath, FullPath),
    lerCSV(FullPath, File),
    append(File, [Data], Saida),
    csv_write_file(FullPath, Saida, [quote(false)]).


% Remover Linha no CSV.
deleteCSV([], _, []).
deleteCSV([UpdatedRow|T], Id, T) :- UpdatedRow =.. [_, Id | _].
deleteCSV([H|T], Id, [H|Out]) :- deleteCSV(T, Id, Out).

deleteRow(FilePath, Id) :-
    atom_concat('Data/', FilePath, FullPath),
    lerCSV(FullPath, File),
    deleteCSV(File, Id, Saida),
    csv_write_file(FullPath, Saida).

% Pega a linha pelo Id.
getRowWithId([], _, []).
getRowWithId([UpdatedRow | _], Id, UpdatedRow) :- UpdatedRow =.. [_, Id | _], !.
getRowWithId([_ | T], Id, UpdatedRow) :- getRowWithId(T, Id, UpdatedRow), !.

getByIdRow(FilePath, Id, ResultRow) :-
    atom_concat('Data/', FilePath, FullPath),
    lerCSV(FullPath, File),
    getRowWithId(File, Id, ResultRow).

getLastRow(FilePath, ResultRow) :-
    atom_concat('Data/', FilePath, FullPath),
    lerCSV(FullPath, File),
    last_Row(File, ResultRow).

% Caso base: o último elemento de uma lista com um único elemento é esse elemento.
last_Row([Row], Row).

% Caso recursivo: o último elemento de uma lista é o mesmo que o último elemento da cauda da lista.
last_Row([_|Resto], Row) :-
    last_Row(Resto, Row).

% Pega a linha pela Matricula.
getRowWithMatricula([], _, []).
getRowWithMatricula([UpdatedRow | _], Matricula, UpdatedRow) :- UpdatedRow =.. [_, _, Matricula | _], !.
getRowWithMatricula([_ | T], Matricula, UpdatedRow) :- getRowWithMatricula(T, Matricula, UpdatedRow), !.

getByMatriculaRow(FilePath, Matricula, ResultRow) :-
    atom_concat('Data/', FilePath, FullPath),
    lerCSV(FullPath, File),
    getRowWithMatricula(File, Matricula, ResultRow).


getAgendamentosRowWithMatricula([], _, []).
getAgendamentosRowWithMatricula([Row | Resto], Matricula, ResultRows) :- 
    parseRow(Row, List),
    ( List = [_, _, _, Matricula | _] ->
        ResultRows = [Row | RestoResultado]
    ; 
        ResultRows = RestoResultado
    ),
    getAgendamentosRowWithMatricula(Resto, Matricula, RestoResultado).

getAgendamentosByMatriculaRow(FilePath, Matricula, ResultRows) :-
    atom_concat('Data/', FilePath, FullPath),
    lerCSV(FullPath, File),
    getAgendamentosRowWithMatricula(File, Matricula, ResultRows).

% Pega a linha pelo IdAgendamento.
getRowWithIdAgendamento([], _, []).
getRowWithIdAgendamento([UpdatedRow | _], IdAgendamento, UpdatedRow) :- UpdatedRow =.. [_, _, _, _, _, IdAgendamento | _], !.
getRowWithIdAgendamento([_ | T], IdAgendamento, UpdatedRow) :- getRowWithIdAgendamento(T, IdAgendamento, UpdatedRow), !.

getByIdAgendamentoRow(FilePath, IdAgendamento, ResultRow) :-
    atom_concat('Data/', FilePath, FullPath),
    lerCSV(FullPath, File),
    getRowWithIdAgendamento(File, IdAgendamento, ResultRow).

% Pega todas as linhas.
getAllRows(FilePath, Rows) :- 
    atom_concat('Data/', FilePath, FullPath),
    lerCSV(FullPath, Rows).

% Atualiza linha dado um id.
updateRow(FilePath, Id, UpdatedData) :-
    atom_concat('Data/', FilePath, FullPath),
    lerCSV(FullPath, File),
    updateCSV(File, Id, UpdatedData, Saida),
    csv_write_file(FullPath, Saida, [quote(false)]).

updateCSV([_|T], -1, UpdatedData, [UpdatedData|T]) :- !.
updateCSV([H|T], Id, UpdatedData, [H|UpdatedTail]) :-
    NextId is Id - 1,
    updateCSV(T, NextId, UpdatedData, UpdatedTail).