:- module(states, [
        screen/1,
        transition/3,
        choices/2,
        changeScreen/3
    ]).

:-consult("./Screens.pl").
:-consult("./Transitions.pl").
:-consult("./Choices.pl").
% LEMBRAR DE REMOVER OS COMENTÁRIOS E TIRAR AUTENTICADO POR
% INSTITUICAO | USUARIO | ADMscreen(agendamentoInstituicao).

changeScreen(CurrentScreen, '0', CurrentScreen) :- !.
changeScreen(CurrentScreen, Choosen, NewScreen) :-
    transition(CurrentScreen, Choosen, NewScreen),
    screen(NewScreen).