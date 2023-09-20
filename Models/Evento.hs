{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Evento (
    Evento(Evento),
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
    , dataEvento :: Day
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
        , "DataEvento" .= show (dataEvento evento)
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
        <*> (parseField =<< r .: "DataEvento")  -- Usar parseField com bind (<>) para analisar DataEvento
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
        , TE.encodeUtf8 (T.pack (show (dataEvento evento)))
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
            let dataEvento = read dataEventoStr :: Day
                inscritos = if inscritosStr == "" then [] else DT.splitOn "," inscritosStr
            return $ Evento nomeEvento instituicaoEvento localEvento dataEvento horaEvento inscritos capacidadeEvento vagasEvento
        | otherwise = fail "Invalid record length"

-- Função para criar um novo evento
criarEvento :: String -> String -> String -> Day -> String -> Int -> IO ()
criarEvento n i l d h capacidade = do
    let vagas = "0/" ++ show capacidade
    let evento = Evento n i l d h [] capacidade vagas
    let fileName = "./Arquivos/Eventos.csv"
    writeEventoCSV fileName evento

-- Função para inscrever um participante em um evento
inscreverParticipante :: String -> String -> IO Bool
inscreverParticipante nomeEvento participante = do
    let fileName = "./Arquivos/Eventos.csv"
    eventos <- readEventoCSV fileName
    case eventos of
        [] -> return False  -- Evento não encontrado
        [evento] -> do
            let capacidadeAtual = capacidade evento
            let inscritosAtuais = inscritos evento
            if length inscritosAtuais < capacidadeAtual
                then do
                    let novosInscritos = participante : inscritosAtuais
                    let novasVagas = show (length novosInscritos) ++ "/" ++ show capacidadeAtual
                    let novoEvento = evento { inscritos = novosInscritos, vagas = novasVagas }
                    writeEventoCSV fileName novoEvento
                    return True  -- Inscrito com sucesso
                else return False  -- Capacidade excedida

-- Função para desinscrever um participante de um evento
desinscreverParticipante :: String -> String -> IO Bool
desinscreverParticipante nomeEvento participante = do
    let fileName = "./Arquivos/Eventos.csv"
    evento <- readEventoCSV fileName
    case evento of
        [] -> return False  -- Evento não encontrado
        [e] -> do
            let inscritosAtuais = inscritos e
            if participante `elem` inscritosAtuais
                then do
                    let novosInscritos = filter (/= participante) inscritosAtuais
                    let novasVagas = show (length novosInscritos) ++ "/" ++ show (capacidade e)
                    let novoEvento = e { inscritos = novosInscritos, vagas = novasVagas }
                    writeEventoCSV fileName novoEvento
                    return True  -- Desinscrito com sucesso
            else return False  -- Participante não encontrado na lista de inscritos

-- Função para ler todos os eventos de um arquivo CSV
readEventoCSV :: FilePath -> IO [Evento]
readEventoCSV fileName = do
    csvData <- BL.readFile fileName
    case decode HasHeader csvData of  -- Corrigido para usar HasHeader
        Left _ -> return []
        Right eventos -> return (V.toList eventos)

-- Função para escrever um evento em um arquivo CSV
writeEventoCSV :: FilePath -> Evento -> IO ()
writeEventoCSV fileName evento = do
    let header = B8.pack "Nome,Instituicao,Local,DataEvento,Hora,Inscritos,Capacidade,Vagas\n"
    let csvContent = encode [evento]
    let finalCsvContent = BL.fromStrict header <> csvContent
    BL.writeFile fileName finalCsvContent