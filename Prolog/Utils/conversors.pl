:- module(conversors, [
    insertAtFirst/3,
    listToRow/2,
    rowsToLists/2,
    split/2,
    listToEvent/2,
    printar/1,
    head/2,
    tail/2,
    normalize/3
    ]).
% Insere o ID como primeiro elemento e converte para Row.
insertAndConvert(ID, List, Row) :-
    insertAtFirst(ID, List, ModifiedList),
    listToRow(ModifiedList, Row).

% Auxiliar para inserir na primeira posição de uma lista.
insertAtFirst(ID, List, [ID|List]).
    
% Converte uma lista para row e vice-versa.
listToRow(Elements, Row) :- Row =.. [row|Elements].
listToEvent(Elements, Event) :- Event =.. [evento|Elements].

% Converte um conjunto de row (tabela), para um conjunto de listas.
rowsToLists([], []).
rowsToLists([Row|Rows], [Elements|Lists]) :-
    listToRow(Elements, Row),
    rowsToLists(Rows, Lists).

% Separa por ',' uma string.
split(String, List) :-
    atomic_list_concat(Atoms, ',', String),
    maplist(atom_string, Atoms, List).

printar(V) :- (var(V); V = ',') -> write(',') ; write(V).

head([X|_], X).
tail([_|X], X).
normalize(Indices, Lists, Result) :-
    maplist(normalize_row(Indices), Lists, Result).

normalize_row(Indices, Row, Result) :-
    normalize_row(Indices, Row, [], Result).

normalize_row([], _, Acc, Result) :-
    atomic_list_concat(Acc, ' ', Result).
normalize_row([Index|Rest], Row, Acc, Result) :-
    nth1(Index, Row, Value),  % Obtém o elemento na coluna especificada pelo índice
    append(Acc, [Value], NewAcc),  % Adiciona o valor ao acumulador
    normalize_row(Rest, Row, NewAcc, Result).