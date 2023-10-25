:- module(modelUsuario, [createUsuario/4]).
:- use_module('../Utils/conversors.pl').


createUsuario(Nome, Matricula, Senha, Usuario) :-
    Usuario = usuario(Matricula,Nome, Senha).