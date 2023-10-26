% Importação services
:- use_module("../Services/Usuario/LoginCadastroService.pl").
:- use_module("../Services/Local/CriarLocalService.pl").

% Importação Repositories
:- use_module("../Repository/agendamentoRepository.pl").
:- use_module("../Repository/eventoRepository.pl").
:- use_module("../Repository/localRepository.pl").

% Importação Models
% :- use_module("../Models/Agendamento/Agendamento.pl").

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
    agendamentoRepository:getAllAgendamento(Agendamentos),
    parseOpcoes([1,2,3], Agendamentos, Opcoes),
    choose(Opcoes, _),
    menu(agendamentoUsuario).

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
    % criarAgendamento(Matricula, IdLocal, Data, Horario)
    menu(agendamentoUsuario).
    
menu(agendamentoUsuarioDeletarScreen) :-
    agendamentoRepository:getAllAgendamento(Agendamentos),
    parseOpcoes([1,2,3], Agendamentos, Opcoes),
    choose(Opcoes, Choosen),
    split_string(Choosen, ' ', ' ', ListChoosen),
    nth1(1, ListChoosen, IdAgendamento),
    writeln(IdAgendamento),
    % deletarAgendamento(Matricula, IdAgendamento)
    menu(agendamentoUsuario).

menu(voltarAgendamentoUsuarioScreen) :-
    writeln("VOLTAR!"),
    menu(main).
     
% =========================================================

% ================= AGENDAMENTO INSTITUIÇÃO ====================

menu(agendamentoInstituicao) :-
    abstract_menu(agendamentoInstituicao, "AGENDAMENTO INSTITUIÇÃO").

menu(agendamentoInstListarScreen) :-
    eventoRepository:getAllEvento(Eventos),
    parseOpcoes([1,2,6,7,8], Eventos, Opcoes),
    choose(Opcoes, _),
    menu(agendamentoInstituicao).

menu(agendamentoInstCriarScreen) :-
    usuarioAtual(Usuario),
    nth1(2, Usuario, Matricula),
    input(['--prompt=Nome do Evento: ', '--placeholder=Digite algo...'], NomeEvento),
    localRepository:getAllLocal(Locais),
    parseOpcoes([1, 2], Locais, Opcoes),
    choose(Opcoes, Local),
    split_string(Local, ' ', ' ', ListLocal),
    nth1(1, ListLocal, IdLocal),
    generate_dates(15, Datas),
    choose(Datas, Data),
    choose(["8 horas", "9 horas", "14 horas", "15 horas"], Horario),
    % criarEvento(Matricula, NomeEvento, IdLocal, Data, Horario)
    menu(agendamentoInstituicao).

menu(agendamentoInstDeletarScreen) :-
    eventoRepository:getAllEvento(Eventos),
    parseOpcoes([1,2,6,7,8], Eventos, Opcoes),
    choose(Opcoes, Choosen),
    split_string(Choosen, ' ', ' ', ListChoosen),
    nth1(1, ListChoosen, IdEvento),
    % deletarEvento(IdEvento)
    menu(agendamentoInstituicao).

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