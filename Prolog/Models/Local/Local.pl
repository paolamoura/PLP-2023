% Inicialização da base de dados vazia
:- dynamic local/4.
:- use_module(library(csv)).

% Predicado para cadastrar um local
cadastrar_local(Id, Nome, Capacidade, Recursos) :-
    \+ local(Id, _, _, _),
    assert(local(Id, Nome, Capacidade, Recursos)),
    salvar_locais_csv.  % Salva os locais em CSV após cada cadastro

% Predicado para atualizar um local (caso ele já exista)
atualizar_local(Id, Nome, Capacidade, Recursos) :-
    retract(local(Id, _, _, _)),
    assert(local(Id, Nome, Capacidade, Recursos)),
    salvar_locais_csv.  % Salva os locais em CSV após a atualização.

% Predicado para atualizar um local (caso ele já exista)
atualizarlocal(Id, Nome, Capacidade, Recursos) :-
    retract(local(Id, _, _, _)),
    assert(local(Id, Nome, Capacidade, Recursos)),
    salvar_locais_csv.  % Salva os locais em CSV após a atualização

% Predicado para visualizar um local existente
visualizar_local(Id) :-
    local(Id, Nome, Capacidade, Recursos),
    write('Local ID: '), write(Id), nl,
    write('Nome: '), write(Nome), nl,
    write('Capacidade: '), write(Capacidade), nl,
    write('Recursos: '), write(Recursos), nl, nl.

% Predicado para visualizar uma lista de locais existentes
visualizar_locais :-
    findall(Id, local(Id, _, _, _), LocaisIds),
    write('Locais Disponíveis:'), nl,
    imprimir_locais(LocaisIds).

% Predicado auxiliar para imprimir a lista de locais
imprimir_locais([]).
imprimir_locais([Id | Resto]) :-
    local(Id, Nome, Capacidade, Recursos),
    write('ID: '), write(Id), nl,
    write('Nome: '), write(Nome), nl,
    write('Capacidade: '), write(Capacidade), nl,
    write('Recursos: '), write(Recursos), nl,
    nl,
    imprimir_locais(Resto).

% Predicado para carregar os locais de um arquivo CSV (se existir)
carregar_locais_csv :-
    retractall(local( _, _, _, _)),  % Remove locais existentes
    csv_read_file('../Data/local.csv', Rows, [functor(local)]),  % Lê o CSV
    maplist(assert, Rows).

% Predicado para salvar os locais em um arquivo CSV
salvar_locais_csv :-
    tell('../Data/local.csv'),
    salvar_locais_recursivo,  % Chama o predicado recursivo para salvar os locais
    told.

% Predicado recursivo para salvar locais
salvar_locais_recursivo :-
    local(Id, Nome, Capacidade, Recursos),
    format('~w,"~w",~w,"~w"~n', [Id, Nome, Capacidade, Recursos]),
    fail.  % Permite continuar a escrever os próximos locais
salvar_locais_recursivo.  % Encerra o predicado recursivo 

% Predicado para deletar um local por ID
deletar_local(Id) :-
    retract(local(Id, _, _, _)),  % Remove o local da base de dados
    salvar_locais_csv.  % Salva os locais restantes no arquivo CSV
 