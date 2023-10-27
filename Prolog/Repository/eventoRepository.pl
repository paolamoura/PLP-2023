:- module(eventoRepository, [saveEvento/1, deleteEventoById/1, getEventoById/2, getAllEvento/1, updateEvento/2, getByAgendamento/4, getEventoByIdInstituicao/2]).
:- use_module("../Data/data.pl").
:- use_module("../Utils/conversors.pl").
:- use_module("../Utils/parsers.pl").

% Fato dinâmico para gerar o id dos agentes
:- dynamic id/1.

% Fato estático para inicializar o ID ao carregar o módulo
:- initialization(loadId).

loadId :-
    path(Path),
    getLastRow(Path, LastRow),
    parseRow(LastRow, LastList),
    (LastList = ['Id','Nome','Instituicao','Local','Data', 'Horario'] ->
        assertz(id(0))
    ;
        primeiro_elemento(LastList, LastId),
        assertz(id(LastId))
    ).

primeiro_elemento([Primeiro|_], Primeiro).

path('eventos.csv').

saveEvento(Evento) :- 
    path(Path),
    id(ID),
    NovoID is ID + 1,
    insertAtFirst(NovoID, Evento, List),
    parseList(List, Row),
    saveRow(Path, Row),
    retractall(id(_)),
    assertz(id(NovoID)).

getByAgendamento(IdLocal,Data, Horario, Evento) :- path(Path), getByAgendamentoRow(Path,IdLocal, Data, Horario, Row), parseRow(Row, Evento).

deleteEventoById(Id) :- path(Path), deleteRow(Path, Id).

getEventoById(Id, Evento) :- path(Path), getByIdRow(Path, Id, Row), parseRow(Row, Evento).

getAllEvento(Eventos) :- path(Path), getAllRows(Path, Rows), parseTable(Rows, Eventos).

getEventoByIdInstituicao(IdInstituicao, Eventos) :- path(Path), getEventosByIdInstituicaoRow(Path, IdInstituicao, Rows), parseTable(Rows, Eventos).

updateEvento(Id, Evento) :- path(Path), listToRow(Evento, Row), updateRow(Path, Id, Row).