:- module(CriarLocalService, [criarLocal/2]).
:- use_module('../../Models/Local/ModelLocal.pl').
:- use_module('../../Repository/localRepository.pl').

criarLocal(Nome, Materiais) :-
    createLocal(Nome, Materiais, Local),
    saveLocal(Local).