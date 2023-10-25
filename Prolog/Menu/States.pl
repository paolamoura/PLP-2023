:- module(states, [
        screen/1,
        transition/3,
        choices/2,
        changeScreen/3
    ]).

% Telas.
screen(main).
screen(cadastro).
screen(login).
screen(agendamentoInstituicao).
screen(agendamentoUsuario).
screen(agendamentoAdm).


% LEMBRAR DE REMOVER OS COMENTÁRIOS E TIRAR AUTENTICADO POR
% INSTITUICAO | USUARIO | ADM

% Condições para Mudanças de tela
transition(main, 'Login', login).
transition(main, 'Cadastro', cadastro).
transition(main, 'Sair', sair).
transition(login, "Autenticado", agendamentoInstituicao).
transition(cadastro, "Autenticado", agendamentoInstituicao).
transition(login, "Autenticado", agendamentoUsuario).
transition(cadastro, "Autenticado", agendamentoUsuario).
transition(login, "Autenticado", agendamentoAdm).
transition(cadastro, "Autenticado", agendamentoAdm).

% Escolhas
choices(main, ['Login','Cadastro','Sair']).
choices(cadastro, ['6','7','8','9','10']).
choices(agendamentoInstituicao, [
    "Listar Eventos",
    "Listar Evento",
    "Criar Evento",
    "Deletar Evento",
    "Voltar"
    ]).
choices(agendamentoUsuario, [
    "Listar Agendamentos",
    "Listar Agendamento",
    "Criar Agendamento",
    "Deletar Agendamento",
    "Voltar"
    ]).
choices(agendamentoAdm, [
    "Listar Locais",
    "Visualizar Local",
    "Criar Local",
    "Visualizar Estatísticas"
    ]).
choices(agendamentosUsuario, [

    ]).

% Regra para mudar tela.
changeScreen(CurrentScreen, '0', CurrentScreen) :- !.
changeScreen(CurrentScreen, Choosen, NewScreen) :-
    transition(CurrentScreen, Choosen, NewScreen),
    screen(NewScreen).