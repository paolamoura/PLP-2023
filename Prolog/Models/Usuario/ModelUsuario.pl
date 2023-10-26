:- module(modelUsuario, [createUsuario/4]).
:- use_module("../../Utils/conversors.pl").

createUsuario(Nome, Matricula, Senha, Usuario) :-
    validarMatricula(Matricula),
    validarSenha(Senha),
    atom_string(Matricula, MatriculaStr),
    atom_string(Senha, SenhaStr),
    Usuario = [MatriculaStr, Nome, SenhaStr].

% Validação da matrícula
validarMatricula(Matricula) :-
    atom_length(Matricula, 9),
    atom_chars(Matricula, [D1, D2, D3, D4 | Rest]),
    (
        member(D1, ['0', '1', '2']),
        number_chars(D2D3, [D2, D3]),
        D2D3 >= 17, D2D3 < 24,
        member(D4, ['1', '2']),
        forall(member(D, Rest), char_type(D, digit))
    ->
        format('Matrícula ~w válida.~n', [Matricula])
    ;
        format('Erro: Matrícula ~w inválida.~n', [Matricula]),
        false % Falso para indicar que a validação falhou
    ).

% Validação da senha
validarSenha(Senha) :-
    atom_length(Senha, N),
    (
        N >= 6,
        atom_chars(Senha, SenhaChars),
        forall(member(C, SenhaChars), char_type(C, ascii))
    ->
        format('Senha ~w válida.~n', [Senha])
    ;
        format('Erro: Senha ~w inválida.~n', [Senha]),
        false % Falso para indicar que a validação falhou
    ).
