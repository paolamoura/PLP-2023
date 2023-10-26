% Importação services
:- use_module("../Services/Usuario/LoginCadastroService.pl").

% Importação Repositories
:- use_module("../Repository/agendamentoRepository.pl").
:- use_module("../Repository/eventoRepository.pl").
% :- use_module("../Repository/localRepository.pl").
% :- use_module("../Repository/usuarioRepository.pl").

% Importação Models
% :- use_module("../Models/Agendamento/Agendamento.pl").

:- use_module("../Utils/gum.pl").
:- use_module("../Utils/conversors.pl").
:- use_module("../Utils/parsers.pl").
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
    writeln("CRIAR Agendamento!"),
    menu(agendamentoUsuario).
    
menu(agendamentoUsuarioDeletarScreen) :-
    writeln("DELETAR Agendamento!"),
    menu(agendamentoUsuario).

menu(voltarAgendamentoUsuarioScreen) :-
    writeln("VOLTAR!"),
    menu(main).
     
% =========================================================

% ================= AGENDAMENTO INSTITUIÇÃO ====================

menu(agendamentoInstituicao) :-
    abstract_menu(agendamentoInstituicao, "AGENDAMENTO INSTITUIÇÃO").

menu(agendamentoInstListarScreen) :-
    agendamentoRepository:getAllEvento(Eventos),
    parseOpcoes([1,2,6, 7, 8], Eventos, Opcoes),
    choose(Opcoes, _),
    menu(agendamentoInstituicao).

menu(agendamentoInstCriarScreen) :-
    writeln("CRIAR Eventos!"),
    menu(agendamentoInstituicao).
    
menu(agendamentoInstDeletarScreen) :-
    writeln("DELETAR Eventos!"),
    menu(agendamentoInstituicao).

menu(voltarAgendamentoInstScreen) :-
    writeln("VOLTAR!"),
    menu(main).

% =========================================================

% ================= AGENDAMENTO ADMINISTRAÇÃO ====================

menu(agendamentoAdm) :-
    abstract_menu(agendamentoAdm, "AGENDAMENTO INSTITUIÇÃO").

menu(agendamentoAdmListarScreen) :-
    writeln("LISTAR LOCAIS!"),
    menu(agendamentoAdm).

menu(agendamentoAdmCriarScreen) :-
    writeln("CRIAR LOCAL!"),
    menu(agendamentoAdm).
    
menu(agendamentoEstatiscaScreen) :-
    writeln("Visualizar Estatística"),
    menu(agendamentoAdm).

menu(voltarAgendamentoAdmScreen) :-
    writeln("VOLTAR!"),
    menu(main).

% =========================================================

:- menu.