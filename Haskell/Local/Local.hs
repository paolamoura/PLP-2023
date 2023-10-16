module Local.Local (Local(..), Locais(Locais),
    --getIdLocal,
    getCapacidadeLocal,
    getLocais,
    getLocaisFromList,
    getLocaisFromLista,
    getLocaisPuros,
    getNomesLocais,
    getNomesLocaisCSV,
    --getLocalPeloId,
    localToString,
    getNomeLocal,
    getLocalPorNome,
    -- getRecursosList,
    getRecursosLocal,
    getRecursosLocais,
    getRecursosLocalToString,
    writeArquivoLocal, criarLocal) where
import System.IO
import System.Directory
import Local.Util
import System.IO.Unsafe (unsafePerformIO)

import Control.Exception
import System.IO.Error
import System.Process
import Control.Monad (when)
import Text.Printf
import Data.List
import Local.Agenda --import da agenda que criamos
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Maybe
import Data.List.Split (chunksOf)

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.Char (isDigit)

data Local = Local {
    --idLocal :: Int,
    nomeLocal :: String,
    recursosLocal :: [String],
    capacidadeLocal :: Int
} deriving (Show, Read)

newtype Locais = Locais {
    locais :: [(Int, Local)]
} deriving Show

criarLocal :: String -> [String] -> Int -> IO ()
criarLocal nome recursos capacidade = do
    let local = Local {
        nomeLocal = nome,
        recursosLocal = recursos,
        capacidadeLocal = capacidade
    }
    writeArquivoLocal local

--------- Local Getters -----------
--getIdLocal :: Local -> Int
--getIdLocal Local {idLocal = i} = i

getNomeLocal :: Local -> String
getNomeLocal Local {nomeLocal = n} = n

-- Função que extrai os nomes dos locais da lista de locais
getNomesLocais :: [Local] -> [String]
getNomesLocais = map (dropPrefix "Nome: ") . map getNomeLocal
  where
    dropPrefix prefix str = if prefix `isPrefixOf` str then drop (length prefix) str else str

getNomesLocaisCSV :: IO [String]
getNomesLocaisCSV = do
    locais <- getLocaisFromList
    return $ getNomesLocais locais

getRecursosLocal :: Local -> [String]
getRecursosLocal Local {recursosLocal = r} = r

getRecursosLocais :: [Local] -> String
getRecursosLocais [] = []
getRecursosLocais (l:ls) = show (getNomeLocal l) ++ "," ++ getRecursosLocalToString (getRecursosLocal l) ++ getRecursosLocais ls

getCapacidadeLocal :: Local -> Int
getCapacidadeLocal Local {capacidadeLocal = c} = c

--getLocalPeloId :: Int -> [Local] -> Maybe Local
--getLocalPeloId id [] = Nothing
--getLocalPeloId id (l:ls) = if id == getIdLocal l then Just l
                        --else getLocalPeloId id ls

getLocaisFromLista :: [(Int, Local)] -> [Local]
getLocaisFromLista [] = []
getLocaisFromLista ((_,l): ls) = l : getLocaisFromLista ls

getLocais :: Locais -> [Local]
getLocais (Locais {locais = l}) = getLocaisFromLista l

getRecursosLocalToString :: [String] -> String
getRecursosLocalToString [] = []
getRecursosLocalToString (r:rs) = if not (null rs) then r ++ "," ++ getRecursosLocalToString rs else r ++ "\n"

localToString :: Local -> String
localToString Local {nomeLocal = n, recursosLocal = r, capacidadeLocal = c} = "Nome: " ++ n ++ "\n" ++
                                                                            "Recursos: " ++ getRecursosLocalToString r  ++
                                                                            "Capacidade: " ++ show c

--------- Escrever Arquivo Local para Usuário ------------

writeArquivoLocal :: Local -> IO()
writeArquivoLocal local = do
    arq <- openFile "./Local/Locais.csv" AppendMode
    arq1 <- openFile "./Local/RecursosLocais.csv" AppendMode

    print local -- mudar isso, mas fazer depois

    writeAgendaLocal (getNomeLocal local)
    let dataRecursosLocal = getRecursosLocalToString (getRecursosLocal local)
    hPutStr arq1 dataRecursosLocal
    hPutStr arq (toWrite local)
    hClose arq
    hClose arq1

toWrite :: Local -> String
toWrite local = localToString local ++ "\n"

--------- Visualizar Local ------------
getLocaisPuros :: [Local]
getLocaisPuros = unsafePerformIO getLocaisFromList :: [Local]

-- convertToList :: [String] -> [Local]
-- convertToList [] = []
-- convertToList (local:list) =
--     convertToLocal (split local ',') : convertToList list

-- convertRecursosToList :: [String] -> [[String]]
-- convertRecursosToList = map (`split` ',')

-- convertToLocal :: [String] -> Maybe Local
-- convertToLocal local
--     | length local >= 4 = Just Local
--         { nomeLocal = local !! 1
--         , recursosLocal = fromIO (getRecursosList nomeLocal)
--         , capacidadeLocal = read (local !! 3) :: Int
--         }
--     | otherwise = Nothing
getLocalPorNome :: String -> IO (Maybe Local)
getLocalPorNome nomeDesejado = do
    locais <- getLocaisFromList
    return $ find (\local -> getNomeLocal local == nomeDesejado) locais

getLocaisFromList :: IO [Local]
getLocaisFromList = do
    contents <- TIO.readFile "./Local/Locais.csv"
    let listaDeLocais = map T.unpack (T.lines contents)
    let groupedLines = chunksOf 3 listaDeLocais
    return $ mapMaybe parseLocal groupedLines

-- Função auxiliar para extrair o valor associado a uma chave
extractKeyValue :: String -> String -> String
extractKeyValue key line = fromMaybe (error $ "Chave não encontrada: " ++ key) $ lookup key keyValuePairs
  where
    keyValuePairs = map (\(k, v) -> (trim k, trim v)) $ map (break (== ':')) $ words line
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- Função auxiliar para tratar erros de conversão
readInt :: String -> Int
readInt s = fromMaybe (error $ "Erro ao analisar capacidadeLine: " ++ s) (readMaybe s)

parseLocal :: [String] -> Maybe Local
parseLocal [nomeLine, recursosLine, capacidadeLine] = do
    let nome = nomeLine
        recursos = words recursosLine
        capacidadeStr = extractKeyValue "Capacidade" capacidadeLine
        capacidade = readInt capacidadeStr

    return Local { nomeLocal = nome, recursosLocal = recursos, capacidadeLocal = capacidade }
parseLocal _ = Nothing

-- getRecursosList :: String -> IO [String]
-- getRecursosList nome = do
--     recursos <- openFile "RecursosLocal.csv" ReadMode
--     listaDeRecursos <- lines <$> hGetContents recursos
--     let recursosTotais = convertRecursosToList listaDeRecursos
--     hClose recursos
--     return $ filtraRecurso nome recursosTotais

filtraRecurso :: String -> [[String]] -> [String]
filtraRecurso _ [] = []
filtraRecurso nome (recurso:recursos)
    | nome == nomeLocal = tail recurso
    | otherwise = filtraRecurso nome recursos
    where
        nomeLocal = read (head recurso) :: String

---- Retirar impurezas do IO ----
fromIO :: IO[String] -> [String]
fromIO x = unsafePerformIO x :: [String]