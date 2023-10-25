:- module(states, [
        screen/1,
        transition/3,
        choices/2,
        changeScreen/3
    ]).

:-use_module("./Menu/AgendamentoUsuario.pl").
:-use_module("./Menu/AgendamentoInstituicao.pl").
:-use_module("./Menu/AgendamentoAdm.pl").
% LEMBRAR DE REMOVER OS COMENTÁRIOS E TIRAR AUTENTICADO POR
% INSTITUICAO | USUARIO | ADM

screen(main).
screen(cadastro).
screen(login).

transition(main, 'Login', login).
transition(main, 'Cadastro', cadastro).
transition(main, 'Sair', sair).

choices(main, ['Login','Cadastro','Sair']).

changeScreen(CurrentScreen, '0', CurrentScreen) :- !.
changeScreen(CurrentScreen, Choosen, NewScreen) :-
    transition(CurrentScreen, Choosen, NewScreen),
    screen(NewScreen).