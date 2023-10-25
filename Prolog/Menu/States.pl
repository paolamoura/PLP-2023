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
screen(agendamentoAdm).

% AGENDAMENTO USUÁRIO
screen(agendamentoUsuario).
screen(agendamentoUsuarioListarScreen).
screen(agendamentoUsuarioCriarScreen).
screen(agendamentoUsuarioDeletarScreen).
screen(voltarAgendamentoUsuarioScreen).

% AGENDAMENTO INSTITUIÇÃO
screen(agendamentoInstituicao).
screen(agendamentoInstListarScreen).
screen(agendamentoInstCriarScreen).
screen(agendamentoInstDeletarScreen).
screen(voltarAgendamentoInstScreen).

% AGENDAMENTO ADM
screen(agendamentoAdmituicao).
screen(agendamentoAdmListarScreen).
screen(agendamentoAdmCriarScreen).
screen(agendamentoAdmDeletarScreen).
screen(voltarAgendamentoAdmScreen).

% LEMBRAR DE REMOVER OS COMENTÁRIOS E TIRAR AUTENTICADO POR
% INSTITUICAO | USUARIO | ADM

% Condições para Mudanças de tela
transition(main, 'Login', login).
transition(main, 'Cadastro', cadastro).
transition(main, 'Sair', sair).

% LOGIN
% transition(login, 'Autenticado', agendamentoUsuario).
% transition(login, 'Autenticado', agendamentoInstituicao).
transition(login, 'Autenticado', agendamentoAdm).

% CADASTRO
% transition(cadastro, 'Autenticado', agendamentoUsuario).
% transition(cadastro, 'Autenticado', agendamentoInstituicao).
transition(cadastro, 'Autenticado', agendamentoAdm).


% AGENDAMENTOS USUÁRIO
transition(agendamentoUsuario, 'Listar Agendamentos', agendamentoUsuarioListarScreen).
transition(agendamentoUsuario, 'Criar Agendamento', agendamentoUsuarioCriarScreen).
transition(agendamentoUsuario, 'Deletar Agendamento', agendamentoUsuarioDeletarScreen).
transition(agendamentoUsuario, 'Voltar', voltarAgendamentoUsuarioScreen).

% AGENDAMENTOS INSTITUIÇÃO
transition(agendamentoInstituicao, 'Listar Eventos', agendamentoInstListarScreen).
transition(agendamentoInstituicao, 'Criar Evento', agendamentoInstCriarScreen).
transition(agendamentoInstituicao, 'Deletar Evento', agendamentoInstDeletarScreen).
transition(agendamentoInstituicao, 'Voltar', voltarAgendamentoInstScreen).

% AGENDAMENTOS ADM
transition(agendamentoAdm, 'Listar Locais', agendamentoAdmListarScreen).
transition(agendamentoAdm, 'Criar Local', agendamentoAdmCriarScreen).
transition(agendamentoAdm, 'Deletar Local', agendamentoAdmDeletarScreen).
transition(agendamentoAdm, 'Voltar', voltarAgendamentoAdmScreen).

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
    "Visualizar Estatísticas",
    "Voltar"
    ]).

% Regra para mudar tela.
changeScreen(CurrentScreen, '0', CurrentScreen) :- !.
changeScreen(CurrentScreen, Choosen, NewScreen) :-
    transition(CurrentScreen, Choosen, NewScreen),
    screen(NewScreen).