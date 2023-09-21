import Utils.Gum
import Utils.Parser
import Utils.GraphicView
import Login.CadastraLogin (fazerCadastro, fazerLogin)
import Login.CadastraLoginInstituicao (fazerCadastro, fazerLogin)
import Models.Usuario (Usuario, confereSenha, getMatricula)
import Models.UsuarioInstituicao (UsuarioInstituicao, confereSenha, getMatricula)
import Local.Local
import Local.Agenda
import Data.Time (Day)
-- Import Services
import Services.AlocarHorarioService (alocarHorarioService)
import Services.DesalocarHorarioService
import Services.CriarLocalService
import Services.CriarEventoService
import Services.InscreverEventoService
import Services.DesinscreverEventoService

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
    putStrLn "Bem-vindo ao programa SGCE-UFCG"
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
    screen <- gum (Choose ["Solicitar Agendamento", "Cancelar Agendamento", "Inscrever em Evento", "Desinscrever em Evento", "Voltar", "Sair"] [])
    case screen of
        "Solicitar Agendamento" -> runSolicitarAgendamento (Left usuario)
        "Cancelar Agendamento" -> runCancelarAgendamento (Left usuario)
        "Inscrever em Evento" -> runInscreverEvento (Left usuario)
        "Desinscrever em Evento" -> runDesinscreverEvento (Left usuario)
        "Voltar" -> runLoginScreen
        "Sair" -> return ExitScreen
        _ -> do
            putStrLn "Opção inválida."
            return ExitScreen

runInscreverEvento :: Either Usuario UsuarioInstituicao -> IO Screen
runInscreverEvento usuario = do
    case usuario of
        Left usuario -> do
            selecionado <- runCommandWithInput "gum table -w 15 -w 5 -w 5 -w 5 -w 5 < ./Arquivos/Eventos.csv" ""
            let parsedData = parseCSV selecionado
            case parsedData of
                [CSVRow [nomeEvento, _, _, _, _, _, _, _]] -> do
                    isDesinscrito <- inscreverParticipanteService nomeEvento (Models.Usuario.getMatricula usuario)
                    if isDesinscrito
                    then putStrLn "Está inscrito!"
                    else putStrLn "Já inscrito" 
                    runAgendamentosUserScreen (eitherToMaybe (Left usuario))
                _ -> putStrLn "Formato CSV Inválido!" >> return ExitScreen
        Right usuario -> do
            putStrLn "Instituição não se inscreve" >> return ExitScreen

runDesinscreverEvento :: Either Usuario UsuarioInstituicao -> IO Screen
runDesinscreverEvento usuario = do
    case usuario of
        Left usuario -> do
            selecionado <- runCommandWithInput "gum table -w 15 -w 5 -w 5 -w 5 -w 5 < ./Arquivos/Eventos.csv" ""
            let parsedData = parseCSV selecionado
            case parsedData of
                [CSVRow [nomeEvento, _, _, _, _, _, _, _]] -> do
                    isDesinscrito <- desinscreverParticipanteService nomeEvento (Models.Usuario.getMatricula usuario)
                    if isDesinscrito
                    then putStrLn "Está desinscrito!"
                    else putStrLn "Já desinscrito" 
                    runAgendamentosUserScreen (eitherToMaybe (Left usuario))
                _ -> putStrLn "Formato CSV Inválido!" >> return ExitScreen
        Right usuario -> do
            putStrLn "Instituição não se desinscreve" >> return ExitScreen
runAgendamentosInstScreen :: Maybe UsuarioInstituicao -> IO Screen
runAgendamentosInstScreen (Just instituicao) = do
    screen <- gum (Choose ["Solicitar Evento", "Voltar", "Sair"] [])
    case screen of
        "Solicitar Evento" -> runCriarEvento (Right instituicao)
        "Voltar" -> runLoginScreen
        "Sair" -> return ExitScreen
        _ -> do
            putStrLn "Opção inválida."
            return ExitScreen

runAgendamentosAdmScreen :: IO Screen
runAgendamentosAdmScreen = do
    screen <- gum (Choose ["Criar Local", "Estatísticas", "Voltar", "Sair"] [])
    case screen of
        "Criar Local" ->  runCriarLocalScreen
        "Estatísticas" -> runEstatiscasScreen
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

runEstatiscasScreen :: IO Screen
runEstatiscasScreen = do
    let locaisPuros = getLocaisPuros
    let nomeDosLocais = getNomesLocais locaisPuros
    local <- gum (Choose nomeDosLocais [])
    printarGraficoGeral
    printarGraficoLocal local
    putStrLn "Aperte Enter para voltar..."
    getLine
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
            selecionado <- gumTable ("./Agenda/" ++ local ++ "Temp.csv") flags
            excluirArquivoTemporario local
            let parsedData = parseCSV selecionado
            case parsedData of
                [CSVRow [nome, time, _, _, _]] -> do
                    alocarHorarioService local nome time (Models.Usuario.getMatricula usuario)
                    runAgendamentosUserScreen (eitherToMaybe (Left usuario))
                _ -> putStrLn "Formato CSV Inválido!" >> return ExitScreen
        Right usuario -> do
            let locaisPuros = getLocaisPuros
            let nomeDosLocais = getNomesLocais locaisPuros
            local <- gum (Choose nomeDosLocais [])
            let flags = [FlagWithArg "-w" "10", FlagWithArg "-w" "5"]
            obterAgendaParaProximosQuinzeDias local
            selecionado <- gumTable ("./Agenda/" ++ local ++ "Temp.csv") flags
            excluirArquivoTemporario local
            let parsedData = parseCSV selecionado
            case parsedData of
                [CSVRow [nome, time, _, _, _]] -> do
                    alocarHorarioService local nome time (Models.UsuarioInstituicao.getMatricula usuario)
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
            selecionado <- gumTable ("./Agenda/" ++ local ++ "Temp.csv") flags
            excluirArquivoTemporario local
            let parsedData = parseCSV selecionado
            case parsedData of
                [CSVRow [nome, time, _, responsavel, _]] -> do
                    desalocaHorarioService local nome time responsavel
                    runAgendamentosUserScreen (eitherToMaybe (Left usuario))
                _ -> putStrLn "Formato CSV Inválido!" >> return ExitScreen
        Right usuario -> do
            let locaisPuros = getLocaisPuros
            let nomeDosLocais = getNomesLocais locaisPuros
            local <- gum (Choose nomeDosLocais [])
            let flags = [FlagWithArg "-w" "10", FlagWithArg "-w" "5"]
            obterAgendaParaProximosQuinzeDias local
            selecionado <- gumTable ("./Agenda/" ++ local ++ "Temp.csv") flags
            excluirArquivoTemporario local
            let parsedData = parseCSV selecionado
            case parsedData of
                [CSVRow [nome, time, _, responsavel, _]] -> do
                    desalocaHorarioService local nome time responsavel
                    runAgendamentosInstScreen (eitherToMaybe (Left usuario))
                _ -> putStrLn "Formato CSV Inválido!" >> return ExitScreen

runCriarEvento :: Either Usuario UsuarioInstituicao -> IO Screen
runCriarEvento instituicao = do
    case instituicao of
        Left instituicao -> do
            putStrLn "Usuário Não Autorizado" >> return ExitScreen
        Right instituicao -> do
            nomeEvento <- gum (Input [FlagWithArg "--prompt" "Nome do Evento: "])
            capacidadeInput <- gum (Input [FlagWithArg "--prompt" "Capacidade: "])
            let capacidade = read capacidadeInput :: Int

            let locaisPuros = getLocaisPuros
            let nomeDosLocais = getNomesLocais locaisPuros
            local <- gum (Choose nomeDosLocais [])
            let flags = [FlagWithArg "-w" "10", FlagWithArg "-w" "5"]
            obterAgendaParaProximosQuinzeDias local
            selecionado <- gumTable ("./Agenda/" ++ local ++ "Temp.csv") flags
            excluirArquivoTemporario local
            let parsedData = parseCSV selecionado
            case parsedData of
                [CSVRow [nome, time, _, _, _]] -> do
                    criarEventoService nomeEvento local (Models.UsuarioInstituicao.getMatricula instituicao) nome time capacidade
                    runAgendamentosInstScreen (eitherToMaybe (Left instituicao))
                _ -> putStrLn "Formato CSV Inválido!" >> return ExitScreen

runExitScreen :: IO Screen
runExitScreen = return ExitScreen