module Menu where

import Utils.Gum
import Data.Char

solicitarLogin :: IO ()
solicitarLogin = do
    username <- gum (Input [FlagWithArg "--prompt" "Usuário: ", FlagWithArg "--placeholder" "..."])
    senha <- gum (Input [FlagWithArg "--prompt" "Senha: ", Flag "--password"])
    -- Chamada do LoginService
    -- fazerLogin username senha
    return ()

solicitarCadastro :: IO ()
solicitarCadastro = do
    matricula <- gum (Input [FlagWithArg "--prompt" "Matrícula: ", FlagWithArg "--placeholder" "..."])
    username <- gum (Input [FlagWithArg "--prompt" "Usuário: ", FlagWithArg "--placeholder" "..."])
    senha <- gum (Input [FlagWithArg "--prompt" "Senha: "])
    confirmSenha <- gum (Input [FlagWithArg "--prompt" "Confirmar Senha: "])
    -- Chamada do LoginService
    -- fazerCadastro matricula username senha confirmSenha
    return ()

selecionarAcao :: String -> IO ()
selecionarAcao escolha
    | trim escolha == "Login" = solicitarLogin
    | trim escolha == "Cadastrar" = solicitarCadastro
    | trim escolha == "Sair" = return()
    where
        trim = filter (not . isSpace)

