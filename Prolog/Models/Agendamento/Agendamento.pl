:- module(Agendamento, [agendar_compromisso/4, desaloca/4]).
:- use_module(library(csv)).
:- dynamic compromisso/5.

%----------------Carregar e salvar a agenda ------------------
% Carrega os compromissos a partir de um arquivo CSV existente
carregar_compromissos :-
    limpar_banco_compromissos,
    open('././Data/agendamentos.csv', read, Stream),
    read_lines(Stream, Lines),
    close(Stream),
    maplist(processar_linha, Lines).

% Limpa o banco de dados de compromissos
limpar_banco_compromissos :-
    retractall(compromisso(_, _, _, _, _)).

% Lê as linhas do arquivo CSV
read_lines(Stream, []) :-
    at_end_of_stream(Stream).
read_lines(Stream, [Line | Rest]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    read_lines(Stream, Rest).

% Processa cada linha do CSV
processar_linha(Line) :-
    atomic_list_concat(SplitLine, ',', Line),
    maplist(atom_string, SplitLine, [IDLocalStr, DataStr, HorarioStr, ResponsavelStr | ListaEsperaStr]),

    atom_number(IDLocalStr, IDLocal),
    atom_string(Data, DataStr),
    atom_string(Horario, HorarioStr),
    atom_string(Responsavel, ResponsavelStr),

    (   ListaEsperaStr = [""]
    ->  ListaEspera = []
    ;   strings_concatenadas(ListaEsperaStrConcatenada, ',', ListaEsperaStr),
        atomic_list_concat(Atoms, ',', ListaEsperaStrConcatenada),
        maplist(atom_string, Atoms, ListaEspera),
        include(\=(''), ListaEspera, ListaEspera)
    ),
    assert(compromisso(IDLocal, Data, Horario, Responsavel, ListaEspera)).


% Converter uma lista de strings em uma lista de átomos
strings_para_atomos([], []).
strings_para_atomos([String | Resto], [Atomo | AtomosResto]) :-
    atom_string(Atomo, String),
    strings_para_atomos(Resto, AtomosResto).

% Concatenar átomos em uma lista com um separador
strings_concatenadas(String, Separador, Lista) :-
    strings_para_atomos(Lista, Atomos),
    atomic_list_concat(Atomos, Separador, String).


% Salva os compromissos no arquivo CSV
salvar_compromissos :-
    findall(compromisso(ID, Data, Horario, Responsavel, ListaEspera), compromisso(ID, Data, Horario, Responsavel, ListaEspera), Compromissos),
    open('././Data/agendamentos.csv', write, Stream, [encoding(utf8)]),
    (   maplist(format_compromisso_line(Stream), Compromissos)
    ->  close(Stream),
        write('Compromissos salvos com sucesso!'), nl
    ;   write('Falha ao salvar compromissos!'), nl
    ).

% Formata um compromisso como uma linha no CSV e escreve no Stream
format_compromisso_line(Stream, compromisso(ID, Data, Horario, Responsavel, ListaEspera)) :-
    atom_string(ID, IDStr),
    atom_string(Data, DataStr),
    atom_string(Horario, HorarioStr),
    atom_string(Responsavel, ResponsavelStr),
    atomic_list_concat(ListaEspera, ',', ListaEsperaStr),
    atomic_list_concat([IDStr, DataStr, HorarioStr, ResponsavelStr, ListaEsperaStr], ',', Line),
    writeln(Stream, Line).

%-------------------Agendamestos------------------------
% Adiciona um novo compromisso à agenda com lista de espera ordenada por matrícula
agendar_compromisso(IDLocal, Data, Horario, NewResponsavel) :-
    compromisso(IDLocal, Data, Horario, Responsavel, ListaEspera),
    \+ meu_member(NewResponsavel, ListaEspera),
    inserir_ordenado_matricula(NewResponsavel, ListaEspera, NovaListaEspera),
    retract(compromisso(IDLocal, Data, Horario, Responsavel, ListaEspera)),
    assert(compromisso(IDLocal, Data, Horario, Responsavel, NovaListaEspera)),
    salvar_compromissos,
    write('Compromisso agendado na lista de espera com sucesso!'), nl,!.

% Adiciona um novo compromisso à agenda se o idLocal, data e horário não existirem
agendar_compromisso(IDLocal, Data, Horario, Responsavel) :-
    \+ compromisso(IDLocal, Data, Horario, _, _),
    assert(compromisso(IDLocal, Data, Horario, Responsavel, [])),
    salvar_compromissos,
    write('Compromisso agendado com sucesso!'), nl.

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
    compromisso(IDLocal, Data, Horario, Responsavel, [ProximoResponsavel|ListaEspera]),
    retract(compromisso(IDLocal, Data, Horario, Responsavel, [ProximoResponsavel|ListaEspera])),
    assert(compromisso(IDLocal, Data, Horario, ProximoResponsavel, ListaEspera)),
    salvar_compromissos,
    write('Compromisso desalocado com sucesso!'), nl,!.

% desaloca se ele for o responsavel e a lista vazia
desalocar(IDLocal, Data, Horario, Responsavel) :-
    compromisso(IDLocal, Data, Horario, Responsavel, []),
    retract(compromisso(IDLocal, Data, Horario, Responsavel, [])),
    salvar_compromissos,
    write('Desalocado com sucesso!'), nl,!.

% Remove o usuário da lista de espera se ele não for o responsável
desalocar(IDLocal, Data, Horario, Usuario) :-
    compromisso(IDLocal, Data, Horario, Responsavel, ListaEspera),
    atomo_para_string(Usuario, UsuarioString),
    meu_member(UsuarioString, ListaEspera),
    remover(UsuarioString ,ListaEspera, NovaListaEspera),
    retract(compromisso(IDLocal, Data, Horario, Responsavel, ListaEspera)),
    assert(compromisso(IDLocal, Data, Horario, Responsavel, NovaListaEspera)),
    salvar_compromissos,
    write('Usuário removido da lista de espera com sucesso!'), nl.

% Converte um átomo em uma string
atomo_para_string(Atom, String) :-
    atom_string(Atom, String).

% Verifica se um elemento está na lista
meu_member(_, []) :- false.
meu_member(X, [X|_]).
meu_member(X, [Y|T]) :- meu_member(X, T).

% Remover elemento da lista
remover(X, [X|C], C):-!.
remover(X, [Y|C], [Y|D]):- remover(X, C, D).

%-------------------Visualização------------------------
% Lista os compromissos agendados
listar_compromissos :-
    findall(compromisso(ID, Data, Horario, Responsavel, ListaEspera), compromisso(ID, Data, Horario, Responsavel, ListaEspera), Compromissos),
    write('ID, Data, Horário, Responsável, Lista de Espera\n'),
    maplist(write_compromisso, Compromissos),
    salvar_compromissos.

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
