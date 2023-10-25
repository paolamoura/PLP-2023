:- module(data, [saveRow/2,
    deleteRow/2,
    getByIdRow/3,
    getAllRows/2,
    updateRow/3,
    getByMatriculaRow/3
    ]).
:- use_module(library(csv)).
:- use_module("../Utils/conversors.pl").

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


getByMatriculaRow(FilePath, Matricula, ResultRow) :-
    atom_concat('Data/', FilePath, FullPath),
    lerCSV(FullPath, File),
    getRowsWithMatricula(File, Matricula, ResultRow).

getRowsWithMatricula([], _, []).
getRowsWithMatricula([Row | Rest], Matricula, ResultRows) :-
    (getRowWithMatricula(Row, Matricula, UpdatedRow) ->
        ResultRows = [UpdatedRow | OtherRows]
    ;
        ResultRows = OtherRows
    ),
    getRowsWithMatricula(Rest, Matricula, OtherRows).

getRowWithMatricula(Row, Matricula, UpdatedRow) :-
    nth1(2, Row, MatriculaNoCSV),  % Obtém o valor da segunda coluna (matrícula)
    writeln('MatriculaNoCSV antes da unificação: '),
    writeln(MatriculaNoCSV),
    MatriculaNoCSV = Matricula,
    writeln('Matricula depois da unificação: '),
    writeln(Matricula),  % Adicione isso para depurar
    UpdatedRow = Row.


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