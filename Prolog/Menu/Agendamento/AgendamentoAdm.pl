:- module(menuAgendamentoAdm, [
    screen/1,
    transition/3,
    choices/2
    ]).

    screen(agendamentoAdm).
screen(agendamentoAdmituicao).
screen(agendamentoAdmListarScreen).
screen(agendamentoAdmCriarScreen).
screen(agendamentoAdmDeletarScreen).
screen(voltarAgendamentoAdmScreen).

transition(login, 'Autenticado', agendamentoAdm).
transition(cadastro, 'Autenticado', agendamentoAdm).
transition(agendamentoAdm, 'Listar Locais', agendamentoAdmListarScreen).
transition(agendamentoAdm, 'Criar Local', agendamentoAdmCriarScreen).
transition(agendamentoAdm, 'Deletar Local', agendamentoAdmDeletarScreen).
transition(agendamentoAdm, 'Voltar', voltarAgendamentoAdmScreen).

choices(agendamentoAdm, [
    "Listar Locais",
    "Criar Local",
    "Visualizar Estatísticas",
    "Voltar"
    ]).