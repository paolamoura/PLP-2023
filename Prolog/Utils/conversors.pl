:- module(conversors, [
    insertAndConvert/3,
    listToRow/2,
    rowsToLists/2
    ]).

insertAndConvert(ID, List, Row) :-
    insertAtFirst(ID, List, ModifiedList),
    listToRow(ModifiedList, Row).

insertAtFirst(ID, List, [ID|List]).
    
listToRow(Elements, Row) :- Row =.. [row|Elements].

rowsToLists([], []).
rowsToLists([Row|Rows], [Elements|Lists]) :-
    listToRow(Elements, Row),
    rowsToLists(Rows, Lists).