:- use_module("../Utils/gum.pl").
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
    writeln("CADASTRO!"),
    input(['--prompt=Matrícula: ', '--placeholder=Digite algo...'], Matricula),
    input(['--password', '--placeholder=Digite a senha...'], Senha),
    input(['--password', '--placeholder=Digite a senha...'], ConfirmarSenha),
    % cadastrarUsuario(Matricula, Senha, ConfirmarSenha, Sessao),
    changeScreen(login, 'Autenticado', NewScreen),
    menu(NewScreen).

menu(login) :-
    writeln("LOGIN!"),
    input(['--prompt=Matrícula: ', '--placeholder=Digite algo...'], Matricula),
    input(['--password', '--placeholder=Digite a senha...'], Senha),
    % autenticar(Matricula, Senha, Sessao),
    changeScreen(login, 'Autenticado', NewScreen),
    menu(NewScreen).

menu(sair) :-
    tty_clear,
    writeln("ATÉ A PRÓXIMA"),
    halt. 

% ========================================================

% ==================== AGENDAMENTO =======================

menu(agendamentoUsuario) :-
    abstract_menu(agendamentoUsuario, "AGENDAMENTO USUÁRIO").

menu(agendamentoInstituicao) :-
    abstract_menu(agendamentoInstituicao, "AGENDAMENTO INSTITUIÇÃO").

menu(agendamentoAdm) :-
    abstract_menu(agendamentoAdm, "AGENDAMENTO ADMINISTRAÇÃO").

menu(agendamentoListarScreen) :-
    writeln("LISTAR!"),
    menu(agendamentoUsuario).

menu(agendamentoCriarScreen) :-
    writeln("CRIAR!"),
    menu(agendamentoUsuario).
    
menu(agendamentoDeletarScreen) :-
    writeln("DELETAR!"),
    menu(agendamentoUsuario).

menu(voltarAgendamentoScreen) :-
    writeln("VOLTAR!"),
    menu(main).
     
% =========================================================

:- menu.