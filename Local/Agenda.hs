{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Local.Agenda (
    AgendaEntry(AgendaEntry),
    geraAgenda,
    aloca,
    writeAgendaLocal,
    printAgenda,
    desaloca,
    contagemDiasDaSemana
    excluirArquivoTemporario,
    obterAgendaParaProximosQuinzeDias
) where

import Data.Csv
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as B8
import Data.List
import qualified Data.Vector as V
import Data.Map (Map)
import Local.Util
import Data.Time.LocalTime
import Data.Ord (comparing)
import Data.Maybe
import System.Directory

-- Definição do tipo de dados AgendaEntry
data AgendaEntry = AgendaEntry
    { date    :: String
    , time    :: String
    , disponibilidade   :: String
    , responsavel :: String
    , listaEspera :: [String]
    } deriving (Generic, Show)

-- Instâncias para trabalhar com CSV
instance ToNamedRecord AgendaEntry where
    toNamedRecord entry = namedRecord
        [ "Data" .= date entry
        , "Hora" .= time entry
        , "Disponibilidade" .= disponibilidade entry
        , "Responsavel" .= responsavel entry
        , "ListaEspera" .= case listaEspera entry of
            [] -> ""  -- Lista vazia, define como string vazia
            _  -> intercalate "," (listaEspera entry)  -- Formata a lista de espera com vírgulas
        ]

instance ToRecord AgendaEntry where
    toRecord entry = record
        [ TE.encodeUtf8 (T.pack (date entry))
        , TE.encodeUtf8 (T.pack (time entry))
        , TE.encodeUtf8 (T.pack (disponibilidade entry))
        , TE.encodeUtf8 (T.pack (responsavel entry))
        , case listaEspera entry of
            [] -> TE.encodeUtf8 (T.pack "")  -- Lista vazia, define como string vazia
            _  -> TE.encodeUtf8 (T.pack (intercalate "," (listaEspera entry)))  -- Formata a lista de espera com vírgulas
        ]

instance FromNamedRecord AgendaEntry where
    parseNamedRecord r = AgendaEntry
        <$> r .: "Data"
        <*> r .: "Hora"
        <*> r .: "Disponibilidade"
        <*> r .: "Responsavel"
        <*> ((\s -> if s == "" then [] else split s ',') <$> r .: "ListaEspera")

instance DefaultOrdered AgendaEntry where
    headerOrder _ = V.fromList
        [ "Data"
        , "Hora"
        , "Disponibilidade"
        , "Responsavel"
        , "ListaEspera"
        ]

instance FromRecord AgendaEntry where
    parseRecord v
        | V.length v == 5 = do
            dateStr <- v .! 0
            timeStr <- v .! 1
            dispStr <- v .! 2
            respStr <- v .! 3
            listaEsperaStr <- v .! 4
            let listaEspera = if listaEsperaStr == "" then [] else split listaEsperaStr ','
            return $ AgendaEntry dateStr timeStr dispStr respStr listaEspera
        | otherwise = fail "Invalid record length"

-- Função para escrever um arquivo de agenda local
writeAgendaLocal :: String -> IO ()
writeAgendaLocal nome = do
    let nomeArquivo = nome
    let csvFilePath = nomeArquivo
    geraAgenda csvFilePath

-- Função para gerar a agenda
geraAgenda :: String -> IO ()
geraAgenda nomeLocal = do
    let year = fromIOInteger getCurrentYear
    let months = [fromIOInt getCurrentMonth..12]
    let day = fromIOInt getCurrentDay
    let timeSlots = ["08:00", "10:00", "12:00", "14:00", "16:00", "18:00", "20:00"]

    let allAgenda = concatMap (\m -> generateAgendaForMonth year m day timeSlots) months

    -- Cabeçalho como um ByteString
    let header = B8.pack "Data,Hora,Disponibilidade,Responsavel,ListaEspera\n"

    -- Conteúdo do CSV
    let csvContent = encode allAgenda

    -- Combine o cabeçalho com o conteúdo do CSV
    let finalCsvContent = BL.fromStrict header <> csvContent

    -- Escreva o conteúdo no arquivo
    BL.writeFile ("./Agenda/" ++ nomeLocal ++ "Agenda.csv") finalCsvContent

    putStrLn "Arquivo CSV da agenda gerado com sucesso!"

-- Função para gerar a agenda para um mês
generateAgendaForMonth :: Integer -> Int -> Int -> [String] -> [AgendaEntry]
generateAgendaForMonth year month day timeSlots = do
    let startDate = fromGregorian year month day
        endDate = addDays (-1) (addGregorianMonthsClip 1 startDate)

    let days = [startDate..endDate]
    [AgendaEntry (formatTime defaultTimeLocale "%d-%m-%Y" day) time
        "Disponivel" "-" [] | day <- days, isDiaUtil day, time <- timeSlots]

-- Função para obter o ano atual
getCurrentYear :: IO Integer
getCurrentYear = do
    currentTime <- getCurrentTime
    let (year, _, _) = toGregorian $ utctDay currentTime
    return year

-- Função para obter o mês atual
getCurrentMonth :: IO Int
getCurrentMonth = do
    currentTime <- getCurrentTime
    let (_, month, _) = toGregorian $ utctDay currentTime
    return month

-- Função para obter o dia atual
getCurrentDay :: IO Int
getCurrentDay = do
    currentTime <- getCurrentTime
    let (_, _, day) = toGregorian $ utctDay currentTime
    return day

-- Função para verificar se um dia é útil (segunda a sexta)
isDiaUtil :: Day -> Bool
isDiaUtil date =
    let (_, _, diaDaSemana) = toWeekDate date
    in diaDaSemana >= 1 && diaDaSemana <= 5

-- Função para converter um IO Integer para Integer
fromIOInteger :: IO Integer -> Integer
fromIOInteger x = unsafePerformIO x :: Integer

-- Função para converter um IO Int para Int
fromIOInt :: IO Int -> Int
fromIOInt x = unsafePerformIO x :: Int

-- Função para atualizar o CSV ao alocar um usuário
alocaUpdateCSV :: FilePath -> String -> String -> String -> IO Bool
alocaUpdateCSV fileName targetDate targetTime newResponsavel = do
    csvData <- BL.readFile fileName
    case decode NoHeader csvData of
        Left err -> do
            putStrLn $ "Erro ao decodificar CSV: " ++ err
            return False
        Right entries -> do
            let updatedEntries = V.map (updateIfNeeded targetDate targetTime newResponsavel) entries
            let updatedCsvContent = encode (V.toList updatedEntries)
            let finalCsvContent = updatedCsvContent
            BL.writeFile fileName finalCsvContent
            return True
  where
    updateIfNeeded targetDate targetTime newResponsavel entry
        | date entry == targetDate && time entry == targetTime =
            if head newResponsavel == '0'
            then entry { disponibilidade = "Ocupado", responsavel = newResponsavel, listaEspera = adicionarElemento (responsavel entry) (listaEspera entry)}
            else if disponibilidade entry == "Disponivel"
                then entry { disponibilidade = "Ocupado", responsavel = newResponsavel }
                else if newResponsavel `notElem` (responsavel entry : listaEspera entry)
                        then entry { listaEspera = adicionarElemento newResponsavel (listaEspera entry) }
                        else entry
        | otherwise = entry

    -- Função para extrair os quatro primeiros dígitos de uma string
    takeFourDigits :: String -> String
    takeFourDigits = take 4

    -- Função para comparar dois números com base nos quatro primeiros dígitos
    compareByFourDigits :: String -> String -> Ordering
    compareByFourDigits = comparing takeFourDigits

    -- Função para realizar o insertion sort com base nos quatro primeiros dígitos
    insertionSortByFourDigits :: [String] -> [String]
    insertionSortByFourDigits = foldr insertByFourDigits []
      where
        insertByFourDigits = insertBy compareByFourDigits

    -- Função para adicionar um elemento a uma lista ordenada por quatro primeiros dígitos
    adicionarElemento :: String -> [String] -> [String]
    adicionarElemento novoElemento lista =
        insertionSortByFourDigits (lista ++ [novoElemento])

-- Função para alocar um usuário na agenda
aloca :: String -> String -> String -> String -> IO ()
aloca nomeLocal dia hora responsavel = do
    let fileName = "./Agenda/" ++ nomeLocal ++ "Agenda.csv"
    success <- alocaUpdateCSV fileName dia hora responsavel
    if success
        then putStrLn "CSV atualizado com sucesso."
        else putStrLn "Falha na atualização do CSV."

contagemDiasDaSemana :: String -> IO [Int]
contagemDiasDaSemana nomeLocal = do
    agenda <- obterAgendaParaProximosTrintaDias nomeLocal
    let contagem = replicate 5 0  -- Lista de 5 zeros para representar os dias da semana
    return $ foldl contarDiasDaSemana contagem agenda
  where
    -- Função para contar os dias da semana em uma entrada da agenda
    contarDiasDaSemana :: [Int] -> AgendaEntry -> [Int]
    contarDiasDaSemana contagem entry =
        case parseData (date entry) of
            Just dataX ->
                let diaSemana = obterDiaSemana dataX
                    listaEsperaDia = listaEspera entry
                in somarListaEspera contagem diaSemana listaEsperaDia
            Nothing -> contagem

    -- Função para obter o dia da semana (Segunda a Sexta)
    obterDiaSemana :: Day -> Int
    obterDiaSemana dataX = 
        let (_, _, diaDaSemana) = toWeekDate dataX
        in case diaDaSemana of
            1 -> 0  -- Segunda
            2 -> 1  -- Terça
            3 -> 2  -- Quarta
            4 -> 3  -- Quinta
            5 -> 4  -- Sexta
            _ -> -1 -- Outro (não deve acontecer)

    -- Função para somar a lista de espera a um dia da semana
    somarListaEspera :: [Int] -> Int -> [String] -> [Int]
    somarListaEspera contagem dia listaEsperaDia =
        if dia >= 0 && dia < 5 -- Certificar-se de que o dia é válido
        then
            let currentCount = contagem !! dia
                pessoasNaLista = length listaEsperaDia
            in take dia contagem ++ [currentCount + pessoasNaLista] ++ drop (dia + 1) contagem
        else
            contagem -- Dia inválido, retornar a contagem sem alterações

-- Função para obter a agenda para os próximos trinta dias
obterAgendaParaProximosQuinzeDias :: String -> IO [AgendaEntry]
obterAgendaParaProximosQuinzeDias nomeLocal = do
    let fileName = "./Agenda/" ++ nomeLocal ++ "Agenda.csv"
    let fileTemp = "./Agenda/" ++ nomeLocal ++ "Temp.csv"

    -- Obter a data atual
    dataAtual <- getCurrentDate

    -- Calcular a data limite (15 dias após a data atual)
    let dataLimite = addDays 15 dataAtual

    -- Ler o arquivo CSV
    csvData <- BL.readFile fileName

    case decode NoHeader csvData of
        Left err -> do
            putStrLn $ "Erro ao decodificar CSV: " ++ err
            return []
        Right entries -> do
            -- Filtrar as entradas da agenda com a data atual e data limite
            let agendaFiltrada = filter (isDataNoIntervalo dataAtual dataLimite) (V.toList entries)
            let agendaByteString = encode agendaFiltrada
            let header = B8.pack "Data,Hora,Disponibilidade,Responsavel,ListaEspera\n"
            let finalCsvContent = BL.fromStrict header <> agendaByteString
            BL.writeFile fileTemp finalCsvContent
            return agendaFiltrada

-- Função para verificar se uma data está dentro de um intervalo
isDataNoIntervalo :: Day -> Day -> AgendaEntry -> Bool
isDataNoIntervalo dataInicio dataFim entry =
    case parseData (date entry) of
        Just dataVerificacao -> dataInicio <= dataVerificacao && dataVerificacao <= dataFim
        Nothing -> False

-- Função para analisar uma data no formato de string "dd-MM-yyyy"
parseData :: String -> Maybe Day
parseData = parseTimeM True defaultTimeLocale "%d-%m-%Y"

-- Função para obter a data atual
getCurrentDate :: IO Day
getCurrentDate = do
    year <- getCurrentYear
    month <- getCurrentMonth
    fromGregorian year month <$> getCurrentDay

-- Função para imprimir a agenda
printAgenda :: String -> IO ()
printAgenda nome = do
    agenda <- obterAgendaParaProximosQuinzeDias nome
    putStrLn "Agenda para os próximos trinta dias:"
    mapM_ printEntry agenda
  where
    printEntry entry = putStrLn $ "date = " ++ show (date entry) ++
                               ", time = " ++ show (time entry) ++
                               ", disponibilidade = " ++ show (disponibilidade entry)

-- Função para atualizar o CSV ao desalocar um usuário
desalocaUpdateCSV :: FilePath -> String -> String -> String -> IO Bool
desalocaUpdateCSV fileName targetDate targetTime targetResponsavel = do
    csvData <- BL.readFile fileName
    case decode NoHeader csvData of
        Left err -> do
            putStrLn $ "Erro ao decodificar CSV: " ++ err
            return False
        Right entries -> do
            let updatedEntries = V.map (updateIfNeeded targetDate targetTime targetResponsavel) entries
            let updatedCsvContent = encode (V.toList updatedEntries)
            let finalCsvContent = updatedCsvContent
            BL.writeFile fileName finalCsvContent
            return True
  where
    updateIfNeeded targetDate targetTime targetResponsavel entry
        | date entry == targetDate && time entry == targetTime && responsavel entry == targetResponsavel =
            if null (listaEspera entry)
            then entry { disponibilidade = newDisponibilidade entry, responsavel = newResponsavel entry }
            else entry { responsavel = head (listaEspera entry), listaEspera = tail (listaEspera entry) }
        | otherwise = entry

    newDisponibilidade entry = "Disponivel"
    newResponsavel entry = "-"

-- Função para desalocar um usuário da agenda
desaloca :: String -> String -> String -> String -> IO ()
desaloca nomeLocal dia hora responsavel = do
    let fileName = "./Agenda/" ++ nomeLocal ++ "Agenda.csv"
    success <- desalocaUpdateCSV fileName dia hora responsavel
    if success
        then putStrLn "CSV atualizado com sucesso."
        else putStrLn "Falha na atualização do CSV."

-- Função para excluir o arquivo temporário
excluirArquivoTemporario :: String -> IO ()
excluirArquivoTemporario nomeLocal = do
    let fileTemp = "./Agenda/" ++ nomeLocal ++ "Temp.csv"
    fileExists <- doesFileExist fileTemp
    if fileExists
        then removeFile fileTemp
        else putStrLn "O arquivo temporário não existe."