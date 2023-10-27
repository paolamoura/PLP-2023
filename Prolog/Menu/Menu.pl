% Importação services
:- use_module("../Services/Usuario/LoginCadastroService.pl").
:- use_module("../Services/Local/CriarLocalService.pl").
:- use_module("../Services/Evento/CriarEventoService.pl").
:- use_module("../Services/Agendamento/CriarAgendamentoService.pl").


% Importação Repositories
:- use_module("../Repository/agendamentoRepository.pl").
:- use_module("../Repository/eventoRepository.pl").
:- use_module("../Repository/localRepository.pl").

% Importação Models
:- use_module("../Models/Agendamento/Agendamento.pl").

:- use_module("../Utils/gum.pl").
:- use_module("../Utils/conversors.pl").
:- use_module("../Utils/parsers.pl").
:- use_module("../Utils/generators.pl").

% Importação Menu
:- use_module("./States.pl").

abstract_menu(CurrentScreen, Header) :-
    tty_clear,
    writeln(Header),
    choices(CurrentScreen, Choices),
    choose(Choices, Choosen),
    changeScreen(CurrentScreen, Choosen, NewScreen),
    menu(NewScreen).

% ==================== MENU PRINCIPAL ======================

menu :- menu(main).
menu(main) :-
    abstract_menu(main, "BEM-VINDO À SGCE-UFCG").

menu(cadastro) :-
    tty_clear,
    writeln("CADASTRO!"),
    input(['--prompt=Matrícula: ', '--placeholder=Digite algo...'], Matricula),
    input(['--prompt=Nome: ', '--placeholder=Digite algo...'], Nome),
    input(['--password', '--placeholder=Digite a senha...'], Senha),
    input(['--password', '--placeholder=Digite a senha...'], ConfirmarSenha),
    cadastro(Matricula, Nome, Senha, ConfirmarSenha),
    changeScreen(cadastro, 'Cadastrado', NewScreen),
    menu(NewScreen).

menu(login) :-
    tty_clear,
    writeln("LOGIN!"),
    input(['--prompt=Matrícula: ', '--placeholder=Digite algo...'], Matricula),
    input(['--password', '--placeholder=Digite a senha...'], Senha),
    login(Matricula, Senha, _, Sessao),
    changeScreen(login, Sessao, NewScreen),
    menu(NewScreen).

menu(sair) :-
    tty_clear,
    writeln("ATÉ A PRÓXIMA"),
    halt.

% ========================================================

% ==================== AGENDAMENTO USUÁRIO =======================

menu(agendamentoUsuario) :-
    abstract_menu(agendamentoUsuario, "AGENDAMENTO USUÁRIO").

menu(agendamentoAdm) :-
    abstract_menu(agendamentoAdm, "AGENDAMENTO ADMINISTRAÇÃO").

menu(agendamentoUsuarioListarScreen) :-
    usuarioAtual(Usuario),
    nth1(2, Usuario, MatriculaAtomo),
    atom_string(MatriculaAtomo, Matricula),
    agendamentoRepository:getAgendamentosByMatriculaRep(Matricula,Agendamentos),
    (Agendamentos = [] -> menu(agendamentoUsuario);
    parseOpcoes([1,2,3,4], Agendamentos, Opcoes),
    choose(Opcoes, _),
    menu(agendamentoUsuario)
    ).

menu(agendamentoUsuarioCriarScreen) :-
    usuarioAtual(Usuario),
    nth1(2, Usuario, Matricula),
    localRepository:getAllLocal(Locais),
    parseOpcoes([1, 2], Locais, Opcoes),
    choose(Opcoes, Local),
    split_string(Local, ' ', ' ', ListLocal),
    nth1(1, ListLocal, IdLocal),
    generate_dates(15, Datas),
    choose(Datas, Data),
    choose(["8 horas", "9 horas", "14 horas", "15 horas"], Horario),
    criarAgendamento(Matricula, IdLocal, Data, Horario),
    menu(agendamentoUsuario).
    
menu(agendamentoUsuarioDeletarScreen) :-
    usuarioAtual(Usuario),
    nth1(2, Usuario, MatriculaAtomo),
    atom_string(MatriculaAtomo, Matricula),
    agendamentoRepository:getAgendamentosByMatriculaRep(Matricula,Agendamentos),
    (Agendamentos = [] -> menu(agendamentoUsuario);
        parseOpcoes([1,2,3,4], Agendamentos, Opcoes),
        choose(Opcoes, Choosen),
        split_string(Choosen, ' ', ' ', ListChoosen),
        nth1(1, ListChoosen, IdLocal),
        nth1(2, ListChoosen, DataAtomo),
        nth1(3, ListChoosen, Hora),
        atom_concat(Hora, " horas", HorarioAtomo),
        atom_string(DataAtomo, Data),
        atom_string(HorarioAtomo, Horario),
        deletarAgendamento(Matricula, IdLocal, Data, Horario),
        menu(agendamentoUsuario)
    ).

menu(voltarAgendamentoUsuarioScreen) :-
    writeln("VOLTAR!"),
    menu(main).
     
% =========================================================

% ================= AGENDAMENTO INSTITUIÇÃO ====================

menu(agendamentoInstituicao) :-
    abstract_menu(agendamentoInstituicao, "AGENDAMENTO INSTITUIÇÃO").

menu(agendamentoInstListarScreen) :-
    usuarioAtual(Usuario),
    nth1(1, Usuario, IdInstituicao),
    eventoRepository:getEventoByIdInstituicao(IdInstituicao, Eventos),
    (Eventos = [] -> menu(agendamentoInstituicao);
    parseOpcoes([1,2,4,5,6], Eventos, Opcoes),
    choose(Opcoes, _),
    menu(agendamentoInstituicao)).

menu(agendamentoInstCriarScreen) :-
    usuarioAtual(Usuario),
    nth1(2, Usuario, Matricula),
    nth1(1, Usuario, IdInstituicao),
    input(['--prompt=Nome do Evento: ', '--placeholder=Digite algo...'], NomeEvento),
    localRepository:getAllLocal(Locais),
    parseOpcoes([1, 2], Locais, Opcoes),
    choose(Opcoes, Local),
    split_string(Local, ' ', ' ', ListLocal),
    nth1(1, ListLocal, IdLocal),
    generate_dates(15, Datas),
    choose(Datas, Data),
    choose(["8 horas", "9 horas", "14 horas", "15 horas"], Horario),
    criarEvento(NomeEvento, IdInstituicao, IdLocal, Matricula, Data, Horario, _),
    menu(agendamentoInstituicao).

menu(agendamentoInstDeletarScreen) :-
    usuarioAtual(Usuario),
    nth1(1, Usuario, IdInstituicao),
    nth1(2, Usuario, MatriculaAtomo),
    eventoRepository:getEventoByIdInstituicao(IdInstituicao, Eventos),
    (Eventos = [] -> menu(agendamentoInstituicao);
    parseOpcoes([1,2,4,5,6], Eventos, Opcoes),
    choose(Opcoes, Choosen),
    split_string(Choosen, ' ', ' ', ListChoosen),
    nth1(1, ListChoosen, IdEvento),
    nth1(3, ListChoosen, IdLocal),
    nth1(4, ListChoosen, DataAtomo),
    nth1(5, ListChoosen, Hora),
    atom_concat(Hora, " horas", HorarioAtomo),
    atom_string(DataAtomo, Data),
    atom_string(HorarioAtomo, Horario),
    atom_string(MatriculaAtomo, Matricula),
    deletarEvento(IdEvento, IdLocal, Matricula, Data, Horario),
    menu(agendamentoInstituicao)).

menu(voltarAgendamentoInstScreen) :-
    writeln("VOLTAR!"),
    menu(main).

% =========================================================

% ================= AGENDAMENTO ADMINISTRAÇÃO ====================

menu(agendamentoAdm) :-
    abstract_menu(agendamentoAdm, "AGENDAMENTO ADMINISTRAÇÃO").

menu(agendamentoAdmListarScreen) :-
    localRepository:getAllLocal(Locais),
    parseOpcoes([1, 2], Locais, Opcoes),
    choose(Opcoes, _),
    menu(agendamentoAdm).

menu(agendamentoAdmCriarScreen) :-
    input(['--prompt=Nome do Local: ', '--placeholder=Digite algo...'], NomeLocal),
    input(['--prompt=Materiais: ', '--placeholder=Bola,Lápis,Rede...'], RawMateriais),
    split(RawMateriais, Materiais),
    criarLocal(NomeLocal,Materiais),
    menu(agendamentoAdm).
    
menu(agendamentoEstatiscaScreen) :-
    writeln("Visualizar Estatística"),
    menu(agendamentoAdm).

menu(voltarAgendamentoAdmScreen) :-
    menu(main).

% =========================================================

:- menu.

