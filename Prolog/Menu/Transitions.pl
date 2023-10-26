:- module(trasitions, [transition/3]).

% MAIN MENU
transition(main, 'Login', login).
transition(main, 'Cadastro', cadastro).
transition(main, 'Sair', sair).

% AGENDAMENTO USUÁRIO
transition(login, 'Autenticado', agendamentoUsuario).
transition(cadastro, 'Autenticado', agendamentoUsuario).
transition(agendamentoUsuario, 'Listar Agendamentos', agendamentoUsuarioListarScreen).
transition(agendamentoUsuario, 'Criar Agendamento', agendamentoUsuarioCriarScreen).
transition(agendamentoUsuario, 'Deletar Agendamento', agendamentoUsuarioDeletarScreen).
transition(agendamentoUsuario, 'Voltar', voltarAgendamentoUsuarioScreen).

% AGENDAMENTO INSTITUIÇÃO
transition(login, 'Autenticado', agendamentoInstituicao).
transition(cadastro, 'Autenticado', agendamentoInstituicao).
transition(agendamentoInstituicao, 'Listar Eventos', agendamentoInstListarScreen).
transition(agendamentoInstituicao, 'Criar Evento', agendamentoInstCriarScreen).
transition(agendamentoInstituicao, 'Deletar Evento', agendamentoInstDeletarScreen).
transition(agendamentoInstituicao, 'Voltar', voltarAgendamentoInstScreen).

% AGENDAMENTO ADMINISTRADOR
transition(login, 'Autenticado', agendamentoAdm).
transition(cadastro, 'Autenticado', agendamentoAdm).
transition(agendamentoAdm, 'Listar Locais', agendamentoAdmListarScreen).
transition(agendamentoAdm, 'Criar Local', agendamentoAdmCriarScreen).
transition(agendamentoAdm, 'Deletar Local', agendamentoAdmDeletarScreen).
transition(agendamentoAdm, 'Voltar', voltarAgendamentoAdmScreen).

