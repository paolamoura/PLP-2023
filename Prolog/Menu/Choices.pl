:- module(choices, [choices/2]).




% MAIN MENU
choices(main, ["Login","Cadastro","Sair"]).

% AGENDAMENTO USUÁRIO
choices(agendamentoUsuario, [
    "Listar Agendamentos",
    "Criar Agendamento",
    "Deletar Agendamento",
    "Voltar"
    ]).

% AGENDAMENTO INSTITUIÇÃO
choices(agendamentoInstituicao, [
    "Listar Eventos",
    "Criar Evento",
    "Deletar Evento",
    "Voltar"
    ]).

% AGENDAMENTO ADMINISTRAÇÃO
choices(agendamentoAdm, [
    "Listar Locais",
    "Criar Local",
    "Visualizar Estatísticas",
    "Voltar"
    ]).