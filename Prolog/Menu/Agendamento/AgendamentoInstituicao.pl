:- module(menuAgendamentoInstituicao, [
    screen/1,
    transition/3,
    choices/2
    ]).




screen(agendamentoInstituicao).
screen(agendamentoInstListarScreen).
screen(agendamentoInstCriarScreen).
screen(agendamentoInstDeletarScreen).
screen(voltarAgendamentoInstScreen).

transition(login, 'Autenticado', agendamentoInstituicao).
transition(cadastro, 'Autenticado', agendamentoInstituicao).
transition(agendamentoInstituicao, 'Listar Eventos', agendamentoInstListarScreen).
transition(agendamentoInstituicao, 'Criar Evento', agendamentoInstCriarScreen).
transition(agendamentoInstituicao, 'Deletar Evento', agendamentoInstDeletarScreen).
transition(agendamentoInstituicao, 'Voltar', voltarAgendamentoInstScreen).

choices(agendamentoInstituicao, [
    "Listar Eventos",
    "Criar Evento",
    "Deletar Evento",
    "Voltar"
    ]).