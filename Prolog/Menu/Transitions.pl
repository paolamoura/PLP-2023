:- module(trasitions, [transition/3]).

% MAIN MENU
transition(main, 'Login', login).
transition(main, 'Cadastro', cadastro).
transition(main, 'Sair', sair).
transition(cadastro, 'Cadastrado', main).

% LOGIN
transition(login, "Usuario", agendamentoUsuario).
transition(login, "Instituicao", agendamentoInstituicao).
transition(login, "ADM", agendamentoAdm).

% AGENDAMENTO USUÁRIO
transition(agendamentoUsuario, 'Listar Agendamentos', agendamentoUsuarioListarScreen).
transition(agendamentoUsuario, 'Criar Agendamento', agendamentoUsuarioCriarScreen).
transition(agendamentoUsuario, 'Deletar Agendamento', agendamentoUsuarioDeletarScreen).
transition(agendamentoUsuario, 'Voltar', voltarAgendamentoUsuarioScreen).

% AGENDAMENTO INSTITUIÇÃO
transition(agendamentoInstituicao, 'Listar Eventos', agendamentoInstListarScreen).
transition(agendamentoInstituicao, 'Criar Evento', agendamentoInstCriarScreen).
transition(agendamentoInstituicao, 'Deletar Evento', agendamentoInstDeletarScreen).
transition(agendamentoInstituicao, 'Voltar', voltarAgendamentoInstScreen).

% AGENDAMENTO ADMINISTRADOR
transition(agendamentoAdm, 'Listar Locais', agendamentoAdmListarScreen).
transition(agendamentoAdm, 'Criar Local', agendamentoAdmCriarScreen).
transition(agendamentoAdm, 'Visualizar Estatística', agendamentoEstatiscaScreen).
transition(agendamentoAdm, 'Voltar', voltarAgendamentoAdmScreen).

