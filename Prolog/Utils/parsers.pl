:- module(parsers, [
    parseRow/2,
    parseTable/2,
    parseList/2
    ]).
:- use_module("../Utils/conversors.pl").

% Faz parse de uma Lista de Elementos para Row.
parseList(Elements, Row) :-
    maplist(parseElement, Elements, ParsedElements),
    listToRow(ParsedElements, Row).

parseElement(Element, ParsedElement) :-
    (is_list(Element) ->
        atomic_list_concat(Element, ',', ParsedElement)
    ;   ParsedElement = Element
    ). 
% Faz parse de uma Lista de Rows para Lista de Listas.
parseTable([], []).
parseTable([Row|Rows], [Elements|Lists]) :-
    parseRow(Row, Elements),
    parseTable(Rows, Lists).

% Faz parse de uma Row para Lista.
parseRow(Row, Data) :-
    listToRow(List, Row),
    maplist(splitIfStringContainsComma, List, Data).

splitIfStringContainsComma(Element, Result) :-
    (   atom(Element), sub_atom(Element, _, _, _, ',') ->
        split(Element, Result)
    ;   Result = Element
    ).
