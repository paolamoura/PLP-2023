:- use_module(library(csv)).
:- dynamic compromisso/5.

%----------------Carregar e salvar a agenda ------------------
% Carrega os compromissos a partir de um arquivo CSV existente
carregar_compromissos :-
    limpar_banco_compromissos,
    csv_read_file('../../Data/agendamentos.csv', Rows, [functor(compromisso), arity(5)]),
    maplist(processar_compromisso, Rows).

% Limpa o banco de dados de compromissos
limpar_banco_compromissos :-
    retractall(compromisso(_, _, _, _, _)).

% Processar cada linha do CSV
processar_compromisso(compromisso(ID, Data, Horario, Responsavel, ListaEsperaStr)) :-
    \+ compromisso(ID, _, _, _, _),
    nonvar(ID),
    nonvar(Data),
    nonvar(Horario),
    nonvar(Responsavel),
    
    (   sub_atom(ListaEsperaStr, _, 1, 0, ',')
    ->  sub_atom(ListaEsperaStr, 0, _, 1, ListaEsperaStrSemVirgula)
    ;   ListaEsperaStrSemVirgula = ListaEsperaStr
    ),
    
    (   ListaEsperaStrSemVirgula = ''
    ->  ListaEspera = []
    ;   Separador = ',',
        atomic_list_concat(Atoms, Separador, ListaEsperaStrSemVirgula),
        maplist(atom_string, Atoms, ListaEspera),
        include(\=(''), ListaEspera, ListaEspera)
    ),
    
    assert(compromisso(ID, Data, Horario, Responsavel, ListaEspera)).

% Salva os compromissos no arquivo CSV
salvar_compromissos :-
    findall(compromisso(ID, Data, Horario, Responsavel, ListaEspera), compromisso(ID, Data, Horario, Responsavel, ListaEspera), Compromissos),
    maplist(format_lista_espera_str, Compromissos, CompromissosStr),
    csv_write_file('../../Data/agendamentos.csv', CompromissosStr).

% Converte a lista de espera em uma string
format_lista_espera_str(compromisso(ID, Data, Horario, Responsavel, ListaEspera), compromisso(ID, Data, Horario, Responsavel, ListaEsperaStr)) :-
    atomic_list_concat(ListaEspera, ',', ListaEsperaStr).

%-------------------Agendamestos------------------------
% Adiciona um novo compromisso à agenda se o idLocal, data e horário não existirem
agendar_compromisso(IDLocal, Data, Horario, Responsavel) :-
    carregar_compromissos,
    \+ compromisso( IDLocal, Data, Horario, _, _),
    assert(compromisso(IDLocal, Data, Horario, Responsavel, [])),
    salvar_compromissos,
    write('Compromisso agendado com sucesso!'), nl,!.

% Adiciona um novo compromisso à agenda com lista de espera ordenada por matrícula
agendar_compromisso(IDLocal, Data, Horario, NewResponsavel) :-
    carregar_compromissos,
    compromisso(IDLocal, Data, Horario, Responsavel, ListaEspera),
    \+ member(NewResponsavel, ListaEspera),
    inserir_ordenado_matricula(NewResponsavel, ListaEspera, NovaListaEspera),
    retract(compromisso(IDLocal, Data, Horario, Responsavel, ListaEspera)),
    assert(compromisso(IDLocal, Data, Horario, Responsavel, NovaListaEspera)),
    salvar_compromissos,
    write('Compromisso agendado na lista de espera com sucesso!'), nl.

% Predicado para inserir um elemento em uma lista ordenada por matrícula
inserir_ordenado_matricula(X, [], [X]).
inserir_ordenado_matricula(X, [H|T], [X|Resto]) :-
    matricula_menor(X, H),
    inserir_ordenado_matricula(H, T, Resto).
inserir_ordenado_matricula(X, [H|T], [H|T1]) :-
    \+ matricula_menor(X, H),
    inserir_ordenado_matricula(X, T, T1).

% Predicado para comparar matrículas pelo critério dos 4 primeiros dígitos
matricula_menor(X, Y) :-
    sub_atom(X, 0, 4, _, DigitsX),
    sub_atom(Y, 0, 4, _, DigitsY),
    DigitsX @< DigitsY.

%-------------------Desalocar------------------------
% Desaloca um compromisso se o usuário for o responsável e a lista não estiver vazia
desalocar(IDLocal, Data, Horario, Responsavel) :-
    carregar_compromissos,
    string_para_numero(Responsavel, NumeroResponsavel), % Converte Responsavel para número
    compromisso(IDLocal, Data, Horario, NumeroResponsavel, [ProximoResponsavel|ListaEspera]),
    retract(compromisso(IDLocal, Data, Horario, NumeroResponsavel, [ProximoResponsavel|ListaEspera])),
    string_para_numero(ProximoResponsavel, NumeroResponsavelProx),
    assert(compromisso(IDLocal, Data, Horario, NumeroResponsavelProx, ListaEspera)),
    salvar_compromissos,
    write('Compromisso desalocado com sucesso!'), nl,!.

% desaloca se ele for o responsavel e a lista vazia
desalocar(IDLocal, Data, Horario, Responsavel) :-
    carregar_compromissos,
    string_para_numero(Responsavel, NumeroResponsavel), % Converte Responsavel para número
    compromisso(IDLocal, Data, Horario, NumeroResponsavel, []),
    retract(compromisso(IDLocal, Data, Horario, NumeroResponsavel, [])),
    salvar_compromissos,
    write('Desalocado com sucesso!'), nl,!.

% Remove o usuário da lista de espera se ele não for o responsável
desalocar(IDLocal, Data, Horario, Usuario) :-
    carregar_compromissos,
    compromisso(IDLocal, Data, Horario, Responsavel, ListaEspera),
    atomo_para_string(Usuario, UsuarioString),
    meu_member(UsuarioString, ListaEspera),
    remover(UsuarioString ,ListaEspera, NovaListaEspera),
    retract(compromisso(IDLocal, Data, Horario, Responsavel, ListaEspera)),
    assert(compromisso(IDLocal, Data, Horario, Responsavel, NovaListaEspera)),
    salvar_compromissos,
    write('Usuário removido da lista de espera com sucesso!'), nl.

% Converte uma string em um número
string_para_numero(String, Numero) :-
    atom_string(Atom, String),
    atom_number(Atom, Numero).

% Converte um átomo em uma string
atomo_para_string(Atom, String) :-
    atom_string(Atom, String).

% Verifica se um elemento está na lista
meu_member(_, []) :- false.
meu_member(X, [X|_]).
meu_member(X, [_|T]) :- meu_member(X, T).

% Remover elemento da lista
remover(X, [X|C], C):-!.
remover(X, [Y|C], [Y|D]):- remover(X, C, D).

%-------------------Visualização------------------------
% Lista os compromissos agendados
listar_compromissos :-
    carregar_compromissos,
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