import Utils.Gum
import Utils.CSVParser
import Login.CadastraLogin (fazerCadastro, fazerLogin)
import Models.Usuario (Usuario, confereSenha, getMatricula)
import Local.Local
import Local.Agenda
-- Import Services
import Services.AlocarHorarioService (alocarHorarioService)
import Services.DesalocarHorarioService

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
    programState <- runProgram MainMenu Nothing

    case programState of
        ExitScreen -> putStrLn "Saindo..."
        _ -> main

runProgram :: Screen -> Maybe Usuario -> IO Screen
runProgram MainMenu _ = do
    screen <- gum (Choose ["Cadastrar", "Login", "Sair"] [])
    case screen of
        "Cadastrar" -> runProgram CadastroScreen Nothing
        "Login" -> runProgram LoginScreen Nothing
        "Sair" -> return ExitScreen
        _ -> do
            putStrLn "Opção inválida."
            runProgram MainMenu Nothing

runProgram CadastroScreen _ = do
    matricula <- gum (Input [FlagWithArg "--prompt" "Matrícula: "])
    nome <- gum (Input [FlagWithArg "--prompt" "Nome: "])
    senha <- gum (Input [FlagWithArg "--prompt" "Senha: ", Flag "--password"])
    confirmacaoSenha <- gum (Input [FlagWithArg "--prompt" "Confirmar Senha: ", Flag "--password"])

    resultadoCadastro <- fazerCadastro matricula nome senha confirmacaoSenha

    case resultadoCadastro of
        Just _ -> do
            putStrLn "Cadastro realizado com sucesso!"
            runProgram MainMenu Nothing
        Nothing -> do
            putStrLn "Falha no cadastro."
            runProgram MainMenu Nothing

runProgram LoginScreen _ = do
    screen <- gum (Choose ["Entrar como Usuário", "Entrar como Instituição", "Entrar como Administrador", "Sair"] [])
    case screen of
        "Entrar como Usuário" -> runProgram LoginUserScreen Nothing
        "Entrar como Instituição" -> runProgram LoginInstScreen Nothing
        "Entrar como Administrador" -> runProgram LoginAdmScreen Nothing
        "Sair" -> runProgram ExitScreen Nothing
        _ -> do
            putStrLn "Opção inválida."
            runProgram MainMenu Nothing

runProgram LoginUserScreen _ = do
    matricula <- gum (Input [FlagWithArg "--prompt" "Matrícula: "])
    senha <- gum (Input [FlagWithArg "--prompt" "Senha: ", Flag "--password"])

    maybeUsuario <- fazerLogin matricula senha

    case maybeUsuario of
        Just usuario -> do
            putStrLn "Login bem-sucedido!"
            runProgram AgendamentosScreen (Just usuario)
        Nothing -> do
            putStrLn "Falha no login."
            runProgram MainMenu Nothing

runProgram AgendamentosScreen (Just usuario) = do
    screen <- gum (Choose [ "Solicitar Agendamento", "Cancelar Agendamento", "Sair"] [])
    case screen of
        "Solicitar Agendamento" -> do
            let locaisPuros = getLocaisPuros
            let nomeDosLocais = getNomesLocais locaisPuros
            local <- gum (Choose nomeDosLocais [])
            let flags = [FlagWithArg "-w" "10", FlagWithArg "-w" "5"]
            selecionado <- gumTable ("./Agenda/" ++ local ++ "Agenda.csv")
            let parsedData = parseCSV selecionado
            case parsedData of
                [CSVRow [date, time, _, _, _]] -> do
                    alocarHorarioService local date time (getMatricula usuario)
                    runProgram ExitScreen (Just usuario)
                _ -> putStrLn "Formato CSV Inválido!" >> runProgram ExitScreen (Just usuario)
        "Cancelar Agendamento" -> do
            let locaisPuros = getLocaisPuros
            let nomeDosLocais = getNomesLocais locaisPuros
            local <- gum (Choose nomeDosLocais [])
            let flags = [FlagWithArg "-w" "10", FlagWithArg "-w" "5"]
            selecionado <- gumTable ("./Agenda/" ++ local ++ "Agenda.csv")
            let parsedData = parseCSV selecionado
            case parsedData of
                [CSVRow [date, time, _, responsavel, _]] -> do
                    desalocaHorarioService local date time responsavel
                    runProgram ExitScreen (Just usuario)
                _ -> putStrLn "Formato CSV Inválido!" >> runProgram ExitScreen (Just usuario)
            runProgram ExitScreen (Just usuario)
        "Sair" -> runProgram ExitScreen (Just usuario)
        _ -> do
            putStrLn "Opção inválida."
            runProgram MainMenu (Just usuario)

runProgram AgendamentosScreen Nothing = do
    putStrLn "Usuário não autenticado."
    runProgram MainMenu Nothing

runProgram LoginInstScreen _ = do
    putStrLn "Login INSTITUIÇÃO"
    runProgram MainMenu Nothing

runProgram LoginAdmScreen _ = do
    putStrLn "Login ADMINISTRAÇÃO"
    runProgram MainMenu Nothing

runProgram ExitScreen _ = return ExitScreen
