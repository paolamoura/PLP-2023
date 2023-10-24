:- module(data, [save/2,
    delete/2,
    getById/3,
    getAllRows/2,
    update/3
    ]).
:- use_module(library(csv)).
:- use_module("../Utils/conversors.pl").

% Lendo o arquivo CSV
lerCSV(FilePath, Rows) :- 
    csv_read_file(FilePath, Rows).

% Salvar Linha no CSV.
save(FilePath, Data) :-
    atom_concat('Data/', FilePath, FullPath),
    lerCSV(FullPath, File),
    append(File, [Data], Saida),
    csv_write_file(FullPath, Saida, [quote(false)]).


% Remover Linha no CSV.
deleteCSV([], _, []).
deleteCSV([UpdatedRow|T], Id, T) :- UpdatedRow =.. [_, Id | _].
deleteCSV([H|T], Id, [H|Out]) :- deleteCSV(T, Id, Out).

delete(FilePath, Id) :-
    atom_concat('Data/', FilePath, FullPath),
    lerCSV(FullPath, File),
    deleteCSV(File, Id, Saida),
    csv_write_file(FullPath, Saida).

% Pega a linha pelo Id.
getRowWithId([], _, []).
getRowWithId([UpdatedRow | _], Id, UpdatedRow) :- UpdatedRow =.. [_, Id | _], !.
getRowWithId([_ | T], Id, UpdatedRow) :- getRowWithId(T, Id, UpdatedRow), !.

getById(FilePath, Id, ResultRow) :-
    atom_concat('Data/', FilePath, FullPath),
    lerCSV(FullPath, File),
    getRowWithId(File, Id, ResultRow).

% Pega todas as linhas.
getAllRows(FilePath, Rows) :- 
    atom_concat('Data/', FilePath, FullPath),
    lerCSV(FullPath, Rows).

% Atualiza linha dado um id.
update(FilePath, Id, UpdatedData) :-
    atom_concat('Data/', FilePath, FullPath),
    lerCSV(FullPath, File),
    updateCSV(File, Id, UpdatedData, Saida),
    csv_write_file(FullPath, Saida, [quote(false)]).

updateCSV([_|T], -1, UpdatedData, [UpdatedData|T]) :- !.
updateCSV([H|T], Id, UpdatedData, [H|UpdatedTail]) :-
    NextId is Id - 1,
    updateCSV(T, NextId, UpdatedData, UpdatedTail).