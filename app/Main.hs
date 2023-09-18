import Utils.Gum
import Login.CadastraLogin (fazerCadastro, fazerLogin)
import Models.Usuario (Usuario, confereSenha)

data Screen = MainMenu
            | CadastroScreen
            | LoginScreen
            | LoginUserScreen
            | LoginInstScreen
            | LoginAdmScreen
            | AgendamentosScreen
            | ExitScreen
            deriving Eq

main :: IO ()
main = do
    putStrLn "Bem-vindo ao programa de cadastro e login!"
    programState <- runProgram MainMenu

    case programState of
        ExitScreen -> putStrLn "Saindo..."
        _ -> main

runProgram :: Screen -> IO Screen
runProgram MainMenu = do
    screen <- gum (Choose ["Cadastrar", "Login", "Sair"] [])
    case screen of
        "Cadastrar" -> runProgram CadastroScreen
        "Login" -> runProgram LoginScreen
        "Sair" -> return ExitScreen
        _ -> do
            putStrLn "Opção inválida."
            runProgram MainMenu

runProgram CadastroScreen = do
    matricula <- gum (Input [FlagWithArg "--prompt" "Matrícula: "])
    nome <- gum (Input [FlagWithArg "--prompt" "Nome: "])
    senha <- gum (Input [FlagWithArg "--prompt" "Senha: ", Flag "--password"])
    confirmacaoSenha <- gum (Input [FlagWithArg "--prompt" "Confirmar Senha: ", Flag "--password"])

    resultadoCadastro <- fazerCadastro matricula nome senha confirmacaoSenha

    case resultadoCadastro of
        Just _ -> do
            putStrLn "Cadastro realizado com sucesso!"
            runProgram MainMenu
        Nothing -> do
            putStrLn "Falha no cadastro."
            runProgram MainMenu

runProgram LoginScreen = do
    screen <- gum (Choose ["Entrar como Usuário", "Entrar como Instituição", "Entrar como Administrador", "Sair"] [])
    case screen of
        "Entrar como Usuário" -> runProgram LoginUserScreen
        "Entrar como Instituição" -> runProgram LoginInstScreen
        "Entrar como Administrador" -> runProgram LoginAdmScreen
        "Sair" -> runProgram ExitScreen
        _ -> do
            putStrLn "Opção inválida."
            runProgram MainMenu

runProgram LoginUserScreen = do
    matricula <- gum (Input [FlagWithArg "--prompt" "Matrícula: "])
    senha <- gum (Input [FlagWithArg "--prompt" "Senha: ", Flag "--password"])

    maybeUsuario <- fazerLogin matricula senha

    case maybeUsuario of
        Just usuario -> do
            putStrLn "Login bem-sucedido!"
            runProgram AgendamentosScreen
        Nothing -> do
            putStrLn "Falha no login."
            runProgram MainMenu

runProgram LoginInstScreen = do
    putStrLn "Login INSTITUIÇÃO"
    runProgram MainMenu

runProgram LoginAdmScreen = do
    putStrLn "Login ADMINISTRAÇÃO"
    runProgram MainMenu



runProgram AgendamentosScreen = do
    screen <- gum (Choose [ "Solicitar Agendamento", "Editar Agendamento", "Cancelar Agendamento","Sair"] [])

    runProgram MainMenu

runProgram ExitScreen = return ExitScreen
