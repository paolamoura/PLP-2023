:- module(parsers, [
    parseRow/2,
    parseTable/2,
    parseList/2,
    remove_nl/2,
    parseOpcoes/3,
    parseMatricula/2,
    parseAllMatricula/2
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

% Remove newline de uma string.
remove_nl(String, Result) :-
    atomic_list_concat(StringList, '\n', String),
    atomic_list_concat(StringList, '', Result).

parseOpcoes(Indexes, Entidade, Opcoes) :- 
    normalize(Indexes, Entidade, Opcoes).

parseAllMatricula([], []).
parseAllMatricula([Row|Rows], [Result|Lists]) :-
    parseRow(Row, Elements),
    parseMatricula(Elements, Result),
    parseAllMatricula(Rows, Lists).

parseMatricula(List, Result) :-
    drop(4, List, Suffix),
    take(4, List, Preffix),
    append(Preffix, [Suffix|[]], Result).

    
drop(N, List, ShorterList) :-
    length(Prefix, N),
    append(Prefix, ShorterList, List).

take(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :- M is N-1, take(M, Xs, Ys).