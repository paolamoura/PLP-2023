import Utils.Gum
import Utils.Parser
import Login.CadastraLogin (fazerCadastro, fazerLogin)
import Login.CadastraLoginInstituicao (fazerCadastro, fazerLogin)
import Models.Usuario (Usuario, confereSenha, getMatricula)
import Models.UsuarioInstituicao (UsuarioInstituicao, confereSenha, getMatricula)
import Local.Local
import Local.Agenda
-- Import Services
import Services.AlocarHorarioService (alocarHorarioService)
import Services.DesalocarHorarioService
import Services.CriarLocalService

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
        "Cadastrar" -> runCadastroScreen
        "Login" -> runLoginScreen
        "Sair" -> return ExitScreen
        _ -> do
            putStrLn "Opção inválida."
            runProgram MainMenu

runCadastroScreen :: IO Screen
runCadastroScreen = do
    screen <- gum (Choose ["Cadastrar como Usuário", "Cadastrar como Instituição", "Voltar", "Sair"] [])
    case screen of
        "Cadastrar como Usuário" -> runCadastroUserScreen
        "Cadastrar como Instituição" -> runCadastroInstScreen
        "Voltar" -> runProgram MainMenu
        "Sair" -> return ExitScreen
        _ -> do
            putStrLn "Opção inválida."
            return ExitScreen

runCadastroUserScreen :: IO Screen
runCadastroUserScreen = do
    matricula <- gum (Input [FlagWithArg "--prompt" "Matrícula: "])
    nome <- gum (Input [FlagWithArg "--prompt" "Nome: "])
    senha <- gum (Input [FlagWithArg "--prompt" "Senha: ", Flag "--password"])
    confirmacaoSenha <- gum (Input [FlagWithArg "--prompt" "Confirmar Senha: ", Flag "--password"])

    resultadoCadastro <- Login.CadastraLogin.fazerCadastro matricula nome senha confirmacaoSenha

    case resultadoCadastro of
        Just _ -> do
            putStrLn "Cadastro realizado com sucesso!"
            runProgram MainMenu
        Nothing -> do
            putStrLn "Falha no cadastro."
            runProgram MainMenu

runCadastroInstScreen :: IO Screen
runCadastroInstScreen = do
    matricula <- gum (Input [FlagWithArg "--prompt" "Código: "])
    nome <- gum (Input [FlagWithArg "--prompt" "Nome: "])
    senha <- gum (Input [FlagWithArg "--prompt" "Senha: ", Flag "--password"])
    confirmacaoSenha <- gum (Input [FlagWithArg "--prompt" "Confirmar Senha: ", Flag "--password"])

    resultadoCadastro <- Login.CadastraLoginInstituicao.fazerCadastro matricula nome senha confirmacaoSenha

    case resultadoCadastro of
        Just _ -> do
            putStrLn "Cadastro realizado com sucesso!"
            runProgram MainMenu
        Nothing -> do
            putStrLn "Falha no cadastro."
            runProgram MainMenu

runLoginScreen :: IO Screen
runLoginScreen = do
    screen <- gum (Choose ["Entrar como Usuário", "Entrar como Instituição", "Entrar como Administrador", "Voltar", "Sair"] [])
    case screen of
        "Entrar como Usuário" -> runLoginUserScreen
        "Entrar como Instituição" -> runLoginInstScreen
        "Entrar como Administrador" -> runLoginAdmScreen
        "Voltar" -> runProgram MainMenu
        "Sair" -> return ExitScreen
        _ -> do
            putStrLn "Opção inválida."
            return ExitScreen

runLoginUserScreen :: IO Screen
runLoginUserScreen = do
    matricula <- gum (Input [FlagWithArg "--prompt" "Matrícula: "])
    senha <- gum (Input [FlagWithArg "--prompt" "Senha: ", Flag "--password"])

    maybeUsuario <- Login.CadastraLogin.fazerLogin matricula senha

    case maybeUsuario of
        Just usuario -> do
            putStrLn "Login bem-sucedido!"
            runAgendamentosUserScreen (Just usuario)
        Nothing -> do
            putStrLn "Falha no login."
            runLoginScreen

runLoginInstScreen :: IO Screen
runLoginInstScreen = do
    matricula <- gum (Input [FlagWithArg "--prompt" "Código: "])
    senha <- gum (Input [FlagWithArg "--prompt" "Senha: ", Flag "--password"])

    maybeUsuario <- Login.CadastraLoginInstituicao.fazerLogin matricula senha

    case maybeUsuario of
        Just usuario -> do
            putStrLn "Login bem-sucedido!"
            runAgendamentosInstScreen (Just usuario)
        Nothing -> do
            putStrLn "Falha no login."
            runLoginScreen

runLoginAdmScreen :: IO Screen
runLoginAdmScreen = do
    senha <- gum (Input [FlagWithArg "--prompt" "Senha: ", Flag "--password"])
    if senha == "99999"
        then do
            putStrLn "Senha correta. Acesso concedido como administrador."
            runAgendamentosAdmScreen
        else do
            putStrLn "Senha incorreta. Acesso negado como administrador."
            runLoginScreen

runAgendamentosUserScreen :: Maybe Usuario -> IO Screen
runAgendamentosUserScreen (Just usuario) = do
    screen <- gum (Choose ["Solicitar Agendamento", "Cancelar Agendamento", "Voltar", "Sair"] [])
    case screen of
        "Solicitar Agendamento" -> runSolicitarAgendamento (Left usuario)
        "Cancelar Agendamento" -> runCancelarAgendamento (Left usuario)
        "Voltar" -> runLoginScreen
        "Sair" -> return ExitScreen
        _ -> do
            putStrLn "Opção inválida."
            return ExitScreen

runAgendamentosInstScreen :: Maybe UsuarioInstituicao -> IO Screen
runAgendamentosInstScreen (Just instituicao) = do
    screen <- gum (Choose ["Solicitar Evento", "Cancelar Evento", "Sair"] [])
    case screen of
        "Solicitar Evento" -> runSolicitarAgendamento (Right instituicao)
        "Cancelar Evento" -> runCancelarAgendamento (Right instituicao)
        "Voltar" -> runLoginScreen
        "Sair" -> return ExitScreen
        _ -> do
            putStrLn "Opção inválida."
            return ExitScreen

runAgendamentosAdmScreen :: IO Screen
runAgendamentosAdmScreen = do
    screen <- gum (Choose ["Criar Local", "Estatísticas", "Solicitar Agendamento", "Cancelar Agendamento","Sair"] [])
    case screen of
        "Criar Local" ->  runCriarLocalScreen
        "Estatísticas" -> putStrLn "Estatísticas mostradas!" >> return ExitScreen
        "Voltar" -> runLoginScreen
        "Sair" -> return ExitScreen

runCriarLocalScreen :: IO Screen
runCriarLocalScreen = do
    nomeLocal <- gum (Input [FlagWithArg "--prompt" "Nome: ", FlagWithArg "--placeholder" "Separados por hífen (-)"])
    recursosInput <- gum (Input [FlagWithArg "--prompt" "Recursos: ", FlagWithArg "--placeholder" "Separados por espaço"])
    let recursos = words recursosInput
    capacidadeInput <- gum (Input [FlagWithArg "--prompt" "Capacidade: "])
    let capacidade = read capacidadeInput :: Int

    criarLocal nomeLocal recursos capacidade

    runAgendamentosAdmScreen

-- Função para a Tela de Solicitar Agendamento
runSolicitarAgendamento :: Either Usuario UsuarioInstituicao -> IO Screen
runSolicitarAgendamento usuario = do
    case usuario of
        Left usuario -> do
            let locaisPuros = getLocaisPuros
            let nomeDosLocais = getNomesLocais locaisPuros
            local <- gum (Choose nomeDosLocais [])
            let flags = [FlagWithArg "-w" "10", FlagWithArg "-w" "5"]
            obterAgendaParaProximosQuinzeDias local
            selecionado <- gumTable ("./Agenda/" ++ local ++ "Temp.csv")
            excluirArquivoTemporario local
            let parsedData = parseCSV selecionado
            case parsedData of
                [CSVRow [date, time, _, _, _]] -> do
                    alocarHorarioService local date time (Models.Usuario.getMatricula usuario)
                    runAgendamentosUserScreen (eitherToMaybe (Left usuario))
                _ -> putStrLn "Formato CSV Inválido!" >> return ExitScreen
        Right usuario -> do
            let locaisPuros = getLocaisPuros
            let nomeDosLocais = getNomesLocais locaisPuros
            local <- gum (Choose nomeDosLocais [])
            let flags = [FlagWithArg "-w" "10", FlagWithArg "-w" "5"]
            obterAgendaParaProximosQuinzeDias local
            selecionado <- gumTable ("./Agenda/" ++ local ++ "Temp.csv")
            excluirArquivoTemporario local
            let parsedData = parseCSV selecionado
            case parsedData of
                [CSVRow [date, time, _, _, _]] -> do
                    alocarHorarioService local date time (Models.UsuarioInstituicao.getMatricula usuario)
                    runAgendamentosInstScreen (eitherToMaybe (Left usuario))
                _ -> putStrLn "Formato CSV Inválido!" >> return ExitScreen

-- Função para a Tela de Cancelar Agendamento
runCancelarAgendamento :: Either Usuario UsuarioInstituicao -> IO Screen
runCancelarAgendamento usuario = do
    case usuario of
        Left usuario -> do
            let locaisPuros = getLocaisPuros
            let nomeDosLocais = getNomesLocais locaisPuros
            local <- gum (Choose nomeDosLocais [])
            let flags = [FlagWithArg "-w" "10", FlagWithArg "-w" "5"]
            obterAgendaParaProximosQuinzeDias local
            selecionado <- gumTable ("./Agenda/" ++ local ++ "Temp.csv")
            excluirArquivoTemporario local
            let parsedData = parseCSV selecionado
            case parsedData of
                [CSVRow [date, time, _, responsavel, _]] -> do
                    desalocaHorarioService local date time responsavel
                    runAgendamentosUserScreen (eitherToMaybe (Left usuario))
                _ -> putStrLn "Formato CSV Inválido!" >> return ExitScreen
        Right usuario -> do
            let locaisPuros = getLocaisPuros
            let nomeDosLocais = getNomesLocais locaisPuros
            local <- gum (Choose nomeDosLocais [])
            let flags = [FlagWithArg "-w" "10", FlagWithArg "-w" "5"]
            obterAgendaParaProximosQuinzeDias local
            selecionado <- gumTable ("./Agenda/" ++ local ++ "Temp.csv")
            excluirArquivoTemporario local
            let parsedData = parseCSV selecionado
            case parsedData of
                [CSVRow [date, time, _, responsavel, _]] -> do
                    desalocaHorarioService local date time responsavel
                    runAgendamentosInstScreen (eitherToMaybe (Left usuario))
                _ -> putStrLn "Formato CSV Inválido!" >> return ExitScreen

runExitScreen :: IO Screen
runExitScreen = return ExitScreen