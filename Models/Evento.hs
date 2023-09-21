{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Evento (
    Evento(..),
    criarEvento,
    inscreverParticipante,
    desinscreverParticipante
) where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Data.Time
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8
import qualified Data.Vector as V
import Data.List
import Data.List.Split as DT

data Evento = Evento
    { nome :: String
    , instituicao :: String
    , local :: String
    , dataEvento :: String
    , hora :: String
    , inscritos :: [String]
    , capacidade :: Int
    , vagas :: String
    } deriving (Generic, Show)

instance FromField Day where
    parseField s = parseTimeM True defaultTimeLocale "%Y-%m-%d" (B8.unpack s)

instance ToNamedRecord Evento where
    toNamedRecord evento = namedRecord
        [ "Nome" .= nome evento
        , "Instituicao" .= instituicao evento
        , "Local" .= local evento
        , "DataEvento" .= dataEvento evento
        , "Hora" .= hora evento
        , "Inscritos" .= intercalate "," (inscritos evento)
        , "Capacidade" .= capacidade evento
        , "Vagas" .= vagas evento
        ]

instance FromNamedRecord Evento where
    parseNamedRecord r = Evento
        <$> r .: "Nome"
        <*> r .: "Instituicao"
        <*> r .: "Local"
        <*> r .: "DataEvento"  -- Usar parseField com bind (<>) para analisar DataEvento
        <*> r .: "Hora"
        <*> ((\s -> if s == "" then [] else DT.splitOn "," s) <$> r .: "Inscritos")
        <*> r .: "Capacidade"
        <*> r .: "Vagas"

instance DefaultOrdered Evento where
    headerOrder _ = V.fromList
        [ "Nome"
        , "Instituicao"
        , "Local"
        , "DataEvento"
        , "Hora"
        , "Inscritos"
        , "Capacidade"
        , "Vagas"
        ]

instance ToRecord Evento where
    toRecord evento = record
        [ TE.encodeUtf8 (T.pack (nome evento))
        , TE.encodeUtf8 (T.pack (instituicao evento))
        , TE.encodeUtf8 (T.pack (local evento))
        , TE.encodeUtf8 (T.pack (dataEvento evento))
        , TE.encodeUtf8 (T.pack (hora evento))
        , TE.encodeUtf8 (T.pack (intercalate "," (inscritos evento)))
        , toField (capacidade evento)
        , TE.encodeUtf8 (T.pack (vagas evento))
        ]

instance FromRecord Evento where
    parseRecord v
        | V.length v == 8 = do
            nomeEvento <- v .! 0
            instituicaoEvento <- v .! 1
            localEvento <- v .! 2
            dataEventoStr <- v .! 3
            horaEvento <- v .! 4
            inscritosStr <- v .! 5
            capacidadeEvento <- v .! 6
            vagasEvento <- v .! 7
            let dataEvento = dataEventoStr
                inscritos = if inscritosStr == "" then [] else DT.splitOn "," inscritosStr
            return $ Evento nomeEvento instituicaoEvento localEvento dataEvento horaEvento inscritos capacidadeEvento vagasEvento
        | otherwise = fail "Invalid record length"

-- Função para criar um novo evento
criarEvento :: String -> String -> String -> String -> String -> Int -> IO Bool
criarEvento n i l d h capacidade = do
    let vagas = "0/" ++ show capacidade
    let novoEvento = Evento n i l d h [] capacidade vagas
    let fileName = "./Arquivos/Eventos.csv"
    eventos <- readEventoCSV fileName
    if eventoComNomeExiste n eventos
        then return False  -- Evento com o mesmo nome já existe
        else do
            let eventosAtualizados = novoEvento : eventos
            writeEventosCSV fileName eventosAtualizados
            return True  -- Evento criado com sucesso

-- Verifica se um evento com o mesmo nome já existe
eventoComNomeExiste :: String -> [Evento] -> Bool
eventoComNomeExiste nomeEvento eventos =
    any (\evento -> nome evento == nomeEvento) eventos

-- Função para inscrever um participante em um evento
inscreverParticipante :: String -> String -> IO Bool
inscreverParticipante nomeEvento participante = do
    let fileName = "./Arquivos/Eventos.csv"
    eventos <- readEventoCSV fileName
    let eventoEncontrado = find (\evento -> nome evento == nomeEvento) eventos
    case eventoEncontrado of
        Nothing -> return False  -- Evento não encontrado
        Just evento -> do
            let capacidadeAtual = capacidade evento
            let inscritosAtuais = inscritos evento
            if length inscritosAtuais < capacidadeAtual && participante `notElem` inscritosAtuais
                then do
                    let novosInscritos = participante : inscritosAtuais
                    let novasVagas = show (length novosInscritos) ++ "/" ++ show capacidadeAtual
                    let novoEvento = evento { inscritos = novosInscritos, vagas = novasVagas }
                    let eventosAtualizados = map (\e -> if nome e == nomeEvento then novoEvento else e) eventos
                    writeEventosCSV fileName eventosAtualizados
                    return True  -- Inscrito com sucesso
                else return False  -- Capacidade excedida ou participante já inscrito-- Capacidade excedida

-- Função para desinscrever um participante de um evento
desinscreverParticipante :: String -> String -> IO Bool
desinscreverParticipante nomeEvento participante = do
    let fileName = "./Arquivos/Eventos.csv"
    eventos <- readEventoCSV fileName
    let eventoEncontrado = find (\evento -> nome evento == nomeEvento) eventos
    case eventoEncontrado of
        Nothing -> return False  -- Evento não encontrado
        Just evento -> do
            let inscritosAtuais = inscritos evento
            if participante `elem` inscritosAtuais
                then do
                    let novosInscritos = filter (/= participante) inscritosAtuais
                    let novasVagas = show (length novosInscritos) ++ "/" ++ show (capacidade evento)
                    let novoEvento = evento { inscritos = novosInscritos, vagas = novasVagas }
                    let eventosAtualizados = map (\e -> if nome e == nomeEvento then novoEvento else e) eventos
                    writeEventosCSV fileName eventosAtualizados
                    return True  -- Desinscrito com sucesso
                else return False  -- Participante não encontrado na lista de inscritos

-- Função para ler todos os eventos de um arquivo CSV
readEventoCSV :: FilePath -> IO [Evento]
readEventoCSV fileName = do
    csvData <- BL.readFile fileName
    case decode HasHeader csvData of
        Left _ -> return []
        Right eventos -> return (V.toList eventos)

-- Função para escrever uma lista de eventos em um arquivo CSV
writeEventosCSV :: FilePath -> [Evento] -> IO ()
writeEventosCSV fileName eventos = do
    let header = B8.pack "Nome,Instituicao,Local,DataEvento,Hora,Inscritos,Capacidade,Vagas\n"
    let csvContent = encode eventos
    let finalCsvContent = BL.fromStrict header <> csvContent
    BL.writeFile fileName finalCsvContent