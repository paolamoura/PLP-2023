:- module(loginCadastro, [login/4, cadastro/4, usuarioAtual/1]).
:- use_module('../../Models/Usuario/GetSetUsuario.pl').
:- use_module('../../Repository/usuarioRepository.pl').
:- use_module('../../Models/Usuario/ModelUsuario.pl').
:- use_module('../../Utils/conversors.pl').

login(Matricula, SenhaRequest, Usuario, Tipo) :-
    getUsuarioSenha(Matricula, Senha),
    sub_atom(Matricula, 0, 1, _, PrimeiroCaractereMatricula),
    (Senha = SenhaRequest ->
        getUsuarioByMatricula(Matricula, Usuario),
        setUsuario(Usuario),
        (PrimeiroCaractereMatricula = '0' ->
            Tipo = "Instituicao"
        ; PrimeiroCaractereMatricula = '1' ->
            Tipo = "Usuario"
        ; PrimeiroCaractereMatricula = '9' ->
            Tipo = "ADM"
        ; % Adicione mais casos conforme necessário
            Tipo = "Desconhecido"
        )
    ; % Senha incorreta
        writeln('Erro: Senha incorreta.'),
        Tipo = "Desconhecido"
    ). 

% Predicado auxiliar para verificar se a matrícula já está cadastrada
matriculaJaCadastrada(Matricula) :-
    getUsuarioByMatricula(Matricula, _).

% Predicado principal de cadastro
cadastro(Matricula,Nome, Senha, Confirmacao) :-
    (matriculaJaCadastrada(Matricula) ->
        writeln('Erro: Matricula já cadastrada!')
    ;
        (Senha = Confirmacao ->
            % Senha e confirmação coincidem
            createUsuario(Nome,Matricula,Senha, Usuario),
            saveUsuario(Usuario),
            writeln('Cadastro bem-sucedido!')
        ;
            % Senha e confirmação não coincidem
            writeln('Erro: Senha e confirmação não coincidem.'),
            writeln('Cadastro não concluído.')
        )
    ).
:- dynamic usuarioAtual/1.

usuarioAtual(nenhum).

setUsuario(Matricula) :- retract(usuarioAtual(_)), assert(usuarioAtual(Matricula)).