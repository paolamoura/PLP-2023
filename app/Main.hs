import Login.CadastraLogin (fazerCadastro, fazerLogin)
import Models.Usuario (Usuario, confereSenha)

main :: IO ()
main = do
    putStrLn "Bem-vindo ao programa de cadastro e login!"
    putStrLn "Por favor, escolha uma opção:"
    putStrLn "1. Cadastrar usuário"
    putStrLn "2. Fazer login"
    putStrLn "3. Sair"

    opcao <- getLine

    case opcao of
        "1" -> do
            putStrLn "Digite a matrícula: "
            matricula <- getLine
            putStrLn "Digite o nome: "
            nome <- getLine
            putStrLn "Digite a senha: "
            senha <- getLine
            putStrLn "Confirme a senha: "
            confirmacaoSenha <- getLine

            resultadoCadastro <- fazerCadastro matricula nome senha confirmacaoSenha

            case resultadoCadastro of
                Just _ -> putStrLn "Cadastro realizado com sucesso!"
                Nothing -> putStrLn "Falha no cadastro."

            main  -- Volta ao menu principal

        "2" -> do
            putStrLn "Digite a matrícula: "
            matricula <- getLine
            putStrLn "Digite a senha: "
            senha <- getLine

            maybeUsuario <- fazerLogin matricula senha

            case maybeUsuario of
                Just usuario -> putStrLn "Login bem-sucedido!"
                Nothing -> putStrLn "Falha no login."

            main  -- Volta ao menu principal

        "3" -> putStrLn "Saindo..."

        _ -> do
            putStrLn "Opção inválida."
            main  -- Volta ao menu principal