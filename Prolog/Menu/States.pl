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
screen(agendamentoListarScreen).
screen(agendamentoCriarScreen).
screen(agendamentoDeletarScreen).
screen(voltarAgendamentoScreen).


% LEMBRAR DE REMOVER OS COMENTÁRIOS E TIRAR AUTENTICADO POR
% INSTITUICAO | USUARIO | ADM

% Condições para Mudanças de tela
transition(main, 'Login', login).
transition(main, 'Cadastro', cadastro).
transition(main, 'Sair', sair).

% LOGIN
transition(login, 'Autenticado', agendamentoUsuario).
% transition(login, 'Autenticado', agendamentoInstituicao).
% transition(login, 'Autenticado', agendamentoAdm).

% CADASTRO
transition(cadastro, 'Autenticado', agendamentoUsuario).
% transition(cadastro, 'Autenticado', agendamentoInstituicao).
% transition(cadastro, 'Autenticado', agendamentoAdm).


% AGENDAMENTOS USUÁRIO
transition(agendamentoUsuario, 'Listar Agendamentos', agendamentoListarScreen).
transition(agendamentoUsuario, 'Criar Agendamento', agendamentoCriarScreen).
transition(agendamentoUsuario, 'Deletar Agendamento', agendamentoDeletarScreen).
transition(agendamentoUsuario, 'Voltar', voltarAgendamentoScreen).

% Escolhas
choices(main, ['Login','Cadastro','Sair']).
choices(agendamentoInstituicao, [
    "Listar Eventos",
    "Criar Evento",
    "Deletar Evento",
    "Voltar"
    ]).
choices(agendamentoUsuario, [
    "Listar Agendamentos",
    "Criar Agendamento",
    "Deletar Agendamento",
    "Voltar"
    ]).
choices(agendamentoAdm, [
    "Listar Locais",
    "Criar Local",
    "Visualizar Estatísticas"
    ]).

% Regra para mudar tela.
changeScreen(CurrentScreen, '0', CurrentScreen) :- !.
changeScreen(CurrentScreen, Choosen, NewScreen) :-
    transition(CurrentScreen, Choosen, NewScreen),
    screen(NewScreen).