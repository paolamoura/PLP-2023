:- module(conversors, [
    insertAtFirst/3,
    listToRow/2,
    rowsToLists/2,
    split/2,
    listToEvent/2
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