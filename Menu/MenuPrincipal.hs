module Menu.MenuPrincipal where
-- Chamada do LoginService
import Login.CadastraLogin
import Models.Usuario
import Utils.Gum
    ( gum, GumCommand(Input), GumFlag(FlagWithArg, Flag) )
import Control.Exception (try, SomeException)
import Data.Char ( isSpace )


-- Função para ler a entrada com tratamento de erro
lerEntrada :: GumFlag -> IO String
lerEntrada flag = do
    resultado <- try (gum (Input [flag])) :: IO (Either SomeException String)
    case resultado of
        Left ex -> do
            putStrLn $ "Erro ao ler a entrada: " ++ show ex
            return ""
        Right input -> return input

solicitarLogin :: IO ()
solicitarLogin = do
    let flags = [FlagWithArg "--prompt" "Matricula Usuário: ", FlagWithArg "--prompt" "Senha: ", Flag "--password"]
    inputs <- mapM lerEntrada flags
    let [matricula, senha] = inputs
    resultado <- fazerLogin matricula senha
    case resultado of
        Just usuario -> putStrLn "Login bem-sucedido"
        Nothing -> putStrLn "Falha no login"

solicitarCadastro :: IO ()
solicitarCadastro = do
    let flags = [FlagWithArg "--prompt" "Matrícula: ", FlagWithArg "--prompt" "Usuário: ", FlagWithArg "--prompt" "Senha: ", FlagWithArg "--prompt" "Confirmar Senha: "]
    inputs <- mapM lerEntrada flags
    let [matricula, username, senha, confirmSenha] = inputs
    resultado <- fazerCadastro matricula username senha confirmSenha
    case resultado of
        Just usuario -> putStrLn "Cadastro bem-sucedido"
        Nothing -> putStrLn "Falha no cadastro"
