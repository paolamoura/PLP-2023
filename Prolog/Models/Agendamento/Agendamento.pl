:- module(Agendamento, [agendar_compromisso/4, salvar_compromissos/0]).
:- use_module(library(csv)).
:- dynamic compromisso/5.  % Definindo a estrutura da agenda: ID, Data, Horário, Responsável, Lista de Espera

% Carrega os compromissos a partir de um arquivo CSV existente
carregar_compromissos :-
    csv_read_file('./Data/agendamentos.csv', Rows, [functor(compromisso), arity(5)]),
    maplist(format_lista_espera_lista, Rows, FormattedCompromissos),
    maplist(assert, FormattedCompromissos).

% Converte a lista de espera de uma string em uma lista
format_lista_espera_lista(compromisso(ID, Data, Horario, Responsavel, ListaEsperaStr), compromisso(ID, Data, Horario, Responsavel, ListaEspera)) :-
    atomic_list_concat(Atoms, ',', ListaEsperaStr),
    maplist(atom_string, Atoms, ListaEspera).

% Salva os compromissos no arquivo CSV
salvar_compromissos :-
    findall(compromisso(ID, Data, Horario, Responsavel, ListaEspera), compromisso(ID, Data, Horario, Responsavel, ListaEspera), Compromissos),
    % Converte a lista de espera de cada compromisso em uma string
    maplist(format_lista_espera_str, Compromissos, CompromissosStr),
    csv_write_file('./Data/agendamentos.csv', CompromissosStr).

% Converte a lista de espera em uma string
format_lista_espera_str(compromisso(ID, Data, Horario, Responsavel, ListaEspera), compromisso(ID, Data, Horario, Responsavel, ListaEsperaStr)) :-
    atomic_list_concat(ListaEspera, ',', ListaEsperaStr).

% Adiciona um novo compromisso à agenda se o idLocal, data e horário não existirem
agendar_compromisso(IDLocal, Data, Horario, Responsavel) :-
    carregar_compromissos,
    % Verifica se o compromisso já existe para a mesma data e horário em algum local
    \+ compromisso( IDLocal, Data, Horario, _, _),
    % Adiciona o compromisso
    assert(compromisso(IDLocal, Data, Horario, Responsavel, [])),
    salvar_compromissos,
    write('Compromisso agendado com sucesso!'), nl,!.

%---------------------------------------------------------------------
% esse adiciona funciona, porem não tem a prioridade na lista de espera!

% Adiciona um novo compromisso à agenda
% agendar_compromisso(IDLocal, Data, Horario, NewResponsavel) :-
    % Verifica se o compromisso já existe para o mesma data e horário no local
    % compromisso(IDLocal, Data, Horario, Responsavel, ListaEspera),
    % Verifica se o responsável não está na lista de espera
    % \+ member(NewResponsavel, ListaEspera),
    % Adiciona o responsável à lista de espera
    % retract(compromisso(IDLocal, Data, Horario, Responsavel, ListaEspera)),
    % assert(compromisso(IDLocal, Data, Horario, Responsavel, [NewResponsavel | ListaEspera])),
    % write('Compromisso agendado na lista de espera com sucesso!'), nl.
%-----------------------------------------------------------------------

% Adiciona um novo compromisso à agenda com prioridade baseada nos 4 primeiros dígitos da matrícula
agendar_compromisso(IDLocal, Data, Horario, NewResponsavel) :-
    % Verifica se o compromisso já existe para a mesma data e horário no local
    compromisso(IDLocal, Data, Horario, Responsavel, ListaEspera), 
    % Verifica se o responsável não está na lista de espera
    \+ member(NewResponsavel, ListaEspera),
    % Adiciona o responsável à lista de espera de forma ordenada
    insert_with_priority(NewResponsavel, [Responsavel | ListaEspera], NewListaEspera),
    % Ordene a lista de espera com base na prioridade
    insertion_sort_priority(NewListaEspera, _),    
    % Remove o compromisso anterior
    retract(compromisso(IDLocal, Data, Horario, Responsavel, ListaEspera)),
    % Adiciona o novo compromisso
    assert(compromisso(IDLocal, Data, Horario, NewResponsavel, NewListaEspera)),
    write('Compromisso agendado na lista de espera com sucesso!'), nl.

% Função para inserir um elemento na lista de espera com base na prioridade mantendo a ordem existente
insert_with_priority(Element, [], [Element]).
insert_with_priority(Element, [H|T], [Element, H|T]) :-
    compare_priority(Element, H, Result),
    Result =< 0,
    !.
insert_with_priority(Element, [H|T], [H|NewT]) :-
    insert_with_priority(Element, T, NewT).

% Função para comparar a prioridade com base nos 4 primeiros dígitos da matrícula
compare_priority(Responsavel1, Responsavel2, Result) :-
    sub_atom(Responsavel1, 0, 4, _, Prefix1),
    sub_atom(Responsavel2, 0, 4, _, Prefix2),
    compare(Result, Prefix1, Prefix2).

% Função para ordenar uma lista com base na prioridade
insertion_sort_priority(List, Sorted) :-
    insertion_sort_priority(List, [], Sorted).
insertion_sort_priority([], Acc, Acc).
insertion_sort_priority([H|T], Acc, Sorted) :-
    insert_with_priority(H, Acc, NewAcc),
    insertion_sort_priority(T, NewAcc, Sorted).

%-----------------------------------------------------------------------

% Desalocar se o usuário for o responsavel e a lista não for vazia
desalocar(IDLocal, Data, Horario, Responsavel) :-
    % Verifica se o compromisso existe para a mesma data e horário em algum local
    compromisso( IDLocal, Data, Horario, Responsavel, [Primeiro|ListaEspera]),
    assert(compromisso(IDLocal, Data, Horario, Primeiro, [ListaEspera])),
    write('Compromisso agendado com sucesso!'), nl,!.

%----------------------------------------------------------------------
% esse desaloca não está deletando a linha inteira no csv! 

% desaloca se ele for o responsavel e a lista vazia
desalocar(IDLocal, Data, Horario, Responsavel) :-
    % Verifica se o compromisso existe para o mesma data e horário no local
    compromisso(IDLocal, Data, Horario, Responsavel, ListaEspera),
    % Adiciona o responsável à lista de espera
    retract(compromisso(IDLocal, Data, Horario, Responsavel, ListaEspera)),
    write('Desalocado com sucesso!'), nl,!.

%-------------------------------------------------------------------
% esse desaloca não foi 100% testado!

% desaloca se ele não for o responsavel e a lista não vazia
desalocar(IDLocal, Data, Horario, NewResponsavel) :-
    % Verifica se o compromisso existe para o mesma data e horário no local
    compromisso(IDLocal, Data, Horario, Responsavel, ListaEspera),
    member(NewResponsavel, ListaEspera),
    remover(NewResponsavel, ListaEspera, NovaLista),
    assert(compromisso(IDLocal, Data, Horario, Responsavel, [NovaLista])),
    write('Desalocado com sucesso!'), nl,!.

% Remover elemento da lista
remover(X, [X|C], C):-!.
remover(X, [Y|C], [Y|D]):-remover(X, C, D).

%-----------------------------------------------------------------------

% Lista os compromissos agendados
listar_compromissos :-
    findall(compromisso(ID, Data, Horario, Responsavel, ListaEspera), compromisso(ID, Data, Horario, Responsavel, ListaEspera), Compromissos),
    write('ID, Data, Horário, Responsável, Lista de Espera\n'),
    maplist(write_compromisso, Compromissos).

write_compromisso(compromisso(ID, Data, Horario, Responsavel, ListaEspera)) :-
    format_lista_espera_write(compromisso(ID, Data, Horario, Responsavel, ListaEspera), ListaEsperaStr),
    write(ID), write(','),
    write(Data), write(','),
    write(Horario), write(','),
    write(Responsavel), write(','),
    write(ListaEsperaStr), nl.

% Converte a lista de espera em uma string
format_lista_espera_write(compromisso(_, _, _, _, ListaEspera), ListaEsperaStr) :-
    atomic_list_concat(ListaEspera, ', ', ListaEsperaStr).

% Obtém o próximo ID disponível
get_next_id(ID) :-
    findall(ID, compromisso(ID, _, _, _, _), IDs),
    max_list(IDs, MaxID),
    ID is MaxID + 1.