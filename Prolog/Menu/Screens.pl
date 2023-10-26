:- module(screen, [screen/1]).


% MAIN MENU
screen(main).
screen(cadastro).
screen(login).
screen(sair).

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

% AGENDAMENTO ADMINISTRADOR
screen(agendamentoAdm).
screen(agendamentoAdmituicao).
screen(agendamentoAdmListarScreen).
screen(agendamentoAdmCriarScreen).
screen(agendamentoEstatiscaScreen).
screen(voltarAgendamentoAdmScreen).
