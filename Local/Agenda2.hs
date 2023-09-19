{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module  Local.Agenda2 (AgendaEntry(AgendaEntry), geraAgenda, atualiza, writeAgendaLocal)where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Data.Time
import Text.Printf (printf)
import Data.Time.Calendar.WeekDate (toWeekDate)
import System.IO.Unsafe
--import qualified Data.ByteString.Lazy.Char8 as BLC
import System.IO
import Control.Exception (handle)
import Data.Ord (comparing)
import Data.Char (ord)
import Control.Monad (mzero)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Char8 as B8
import Data.Maybe
import Data.List
import qualified Data.Vector as V
import Haskell.Util
import System.Directory

data AgendaEntry = AgendaEntry
    { date    :: String
    , time    :: String
    , disponibilidade   :: String
    , responsavel :: String
    , listaEspera :: [String]
    } deriving (Generic)

--instance ToNamedRecord AgendaEntry where
    --toNamedRecord entry = namedRecord
        --[ TE.encodeUtf8 (T.pack "Data") .= TE.encodeUtf8 (T.pack (date entry))
        --, TE.encodeUtf8 (T.pack "Hora") .= TE.encodeUtf8 (T.pack (time entry))
        --, TE.encodeUtf8 (T.pack "Disponibilidade") .= TE.encodeUtf8 (T.pack (disponibilidade entry))
        --, TE.encodeUtf8 (T.pack "Responsavel") .= TE.encodeUtf8 (T.pack (responsavel entry))
        --, TE.encodeUtf8 (T.pack "ListaEspera") .= TE.encodeUtf8 (T.pack (concat (listaEspera entry)))  -- Converte a lista em uma única string
        --]
instance ToNamedRecord AgendaEntry where
    toNamedRecord entry = namedRecord
        [ TE.encodeUtf8 (T.pack "Data") .= TE.encodeUtf8 (T.pack (date entry))
        , TE.encodeUtf8 (T.pack "Hora") .= TE.encodeUtf8 (T.pack (time entry))
        , TE.encodeUtf8 (T.pack "Disponibilidade") .= TE.encodeUtf8 (T.pack (disponibilidade entry))
        , TE.encodeUtf8 (T.pack "Responsavel") .= TE.encodeUtf8 (T.pack (responsavel entry))
        , TE.encodeUtf8 (T.pack "ListaEspera") .= TE.encodeUtf8 (T.pack (formatListaEspera (listaEspera entry))) -- Formata a lista de espera
        ]

formatListaEspera :: [String] -> String
formatListaEspera = intercalate "/" -- Usando intercalate para unir a lista com '/'

instance ToRecord AgendaEntry where
    toRecord entry = record
        [ TE.encodeUtf8 (T.pack (date entry))
        , TE.encodeUtf8 (T.pack (time entry))
        , TE.encodeUtf8 (T.pack (disponibilidade entry))
        , TE.encodeUtf8 (T.pack (responsavel entry))
        , TE.encodeUtf8 (T.pack (concat (listaEspera entry)))  -- Converte a lista em uma única string
        ]

instance FromNamedRecord AgendaEntry where
    parseNamedRecord r = AgendaEntry
        <$> r .: "Data"
        <*> r .: "Hora"
        <*> r .: "Disponibilidade"
        <*> r .: "Responsavel"
        <*> (words <$> r .: "ListaEspera")

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
            let listaEspera = split listaEsperaStr ',' 
            return $ AgendaEntry dateStr timeStr dispStr respStr listaEspera
        | otherwise = fail "Invalid record length"

--------- Escrever arquivo de agenda do local -----------

writeAgendaLocal :: String -> IO ()
writeAgendaLocal nome = do
    let nomeArquivo = nome
    let csvFilePath = nomeArquivo
    geraAgenda csvFilePath

geraAgenda :: String -> IO ()
geraAgenda nome = do
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
    BL.writeFile (nome ++ "Agenda.csv") finalCsvContent

    putStrLn "Arquivo CSV da agenda gerado com sucesso!"


generateAgendaForMonth :: Integer -> Int -> Int -> [String] -> [AgendaEntry]
generateAgendaForMonth year month day timeSlots = do
    let startDate = fromGregorian year month day
        endDate = addDays (-1) (addGregorianMonthsClip 1 startDate)

    let days = [startDate..endDate]
    [AgendaEntry (formatTime defaultTimeLocale "%d-%m-%Y" day) time
        "Disponivel" "-" []| day <- days,  isDiaUtil day, time <- timeSlots]

--Função para obter o ano atual
getCurrentYear :: IO Integer
getCurrentYear = do
    currentTime <- getCurrentTime
    let (year, _, _) = toGregorian $ utctDay currentTime
    return year

getCurrentMonth :: IO Int
getCurrentMonth = do
    currentTime <- getCurrentTime
    let (_, moth, _) = toGregorian $ utctDay currentTime
    return moth

getCurrentDay :: IO Int
getCurrentDay = do
    currentTime <- getCurrentTime
    let (_, _, day) = toGregorian $ utctDay currentTime
    return day

--Função para verificar se um dia é útil (segunda a sexta)
isDiaUtil :: Day -> Bool
isDiaUtil date =
    let (_, _, diaDaSemana) = toWeekDate date
    in diaDaSemana >= 1 && diaDaSemana <= 5

fromIOInteger :: IO Integer -> Integer
fromIOInteger x = unsafePerformIO x :: Integer

fromIOInt :: IO Int -> Int
fromIOInt x = unsafePerformIO x :: Int

--aheader :: V.Vector Name
--aheader = V.fromList ["Data", "Hora", "Disponibilidade", "Responsavel", "ListaEspera"]

toAgendaEntry :: [String] -> AgendaEntry
toAgendaEntry [dateStr, timeStr, dispStr, respStr, listaEsperaStr] = 
    AgendaEntry
        { date = dateStr
        , time = timeStr
        , disponibilidade = dispStr
        , responsavel = respStr
        , listaEspera = words listaEsperaStr  -- Divide a string em uma lista de strings
        }

--updateCSV :: FilePath -> String -> String -> String -> String -> IO Bool
--updateCSV fileName targetDate targetTime newDisponibilidade newResponsavel = do
    --csvData <- BL.readFile fileName
    --case decode NoHeader csvData of
        --Left err -> do
            --putStrLn $ "Erro ao decodificar CSV: " ++ err
            --return False
        --Right entries -> do
            --let updatedEntries = V.map (updateIfNeeded targetDate targetTime newDisponibilidade newResponsavel) entries
            
            -- Verifique se o arquivo já existe
            --fileExists <- doesFileExist fileName
            
            --let updatedCsvContent = encode (V.toList updatedEntries)
            
            -- Se o arquivo não existe, adicione o cabeçalho manualmente
            --let finalCsvContent = if fileExists
                                    --then updatedCsvContent
                                    --else BL.append (encodeByName aheader (V.toList entries)) updatedCsvContent
            
            --BL.writeFile fileName finalCsvContent
            --return True
  --where
    --updateIfNeeded targetDate targetTime newDisponibilidade newResponsavel entry | date entry == targetDate && time entry == targetTime =
            --if disponibilidade entry == "Disponivel"
            --then entry { disponibilidade = "OCUPADO", responsavel = newResponsavel }
            --else entry { listaEspera = listaEspera entry ++ [newResponsavel] }
        -- otherwise = entry


updateCSV :: FilePath -> String -> String -> String -> IO Bool
updateCSV fileName targetDate targetTime newResponsavel = do
    csvData <- BL.readFile fileName
    case decode NoHeader csvData of
        Left err -> do
            putStrLn $ "Erro ao decodificar CSV: " ++ err
            return False
        Right entries -> do
            let updatedEntries = V.map (updateIfNeeded targetDate targetTime newResponsavel) entries
            
            -- Verifique se o arquivo já existe
            --fileExists <- doesFileExist fileName
            
            let updatedCsvContent = encode (V.toList updatedEntries)
            let finalCsvContent = updatedCsvContent
            -- Se o arquivo não existe, adicione o cabeçalho manualmente
            --let finalCsvContent = if fileExists
                                    --then updatedCsvContent
                                    --else BL.append (encodeByName aheader (V.toList entries)) updatedCsvContent
            
            BL.writeFile fileName finalCsvContent
            return True
  where
    updateIfNeeded targetDate targetTime newResponsavel entry
        | date entry == targetDate && time entry == targetTime =
            if disponibilidade entry == "Disponivel"
            then entry { disponibilidade = calculateNewDisponibilidade entry, responsavel = newResponsavel }
            else entry { listaEspera = listaEspera entry ++ [newResponsavel] }
        | otherwise = entry
    
    calculateNewDisponibilidade entry
        | disponibilidade entry == "Disponivel" = "Ocupado"
        | otherwise = disponibilidade entry

atualiza :: String -> String -> String -> String -> IO ()
atualiza nome dia hora responsavel = do
    let fileName = nome ++ "Agenda.csv"
    success <- updateCSV fileName dia hora responsavel
    if success
        then putStrLn "CSV atualizado com sucesso."
        else putStrLn "Falha na atualização do CSV."