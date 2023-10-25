:- module(menuAgendamentoUsuario, [
    screen/1,
    transition/3,
    choices/2
    ]).

screen(agendamentoUsuario).
screen(agendamentoUsuarioListarScreen).
screen(agendamentoUsuarioCriarScreen).
screen(agendamentoUsuarioDeletarScreen).
screen(voltarAgendamentoUsuarioScreen).

transition(login, 'Autenticado', agendamentoUsuario).
transition(cadastro, 'Autenticado', agendamentoUsuario).
transition(agendamentoUsuario, 'Listar Agendamentos', agendamentoUsuarioListarScreen).
transition(agendamentoUsuario, 'Criar Agendamento', agendamentoUsuarioCriarScreen).
transition(agendamentoUsuario, 'Deletar Agendamento', agendamentoUsuarioDeletarScreen).
transition(agendamentoUsuario, 'Voltar', voltarAgendamentoUsuarioScreen).

choices(agendamentoUsuario, [
    "Listar Agendamentos",
    "Criar Agendamento",
    "Deletar Agendamento",
    "Voltar"
    ]).