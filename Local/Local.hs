module Local.Local (Local(Local), Locais(Locais),
    --getIdLocal,
    getCapacidadeLocal,
    getLocais,
    getLocaisFromList,
    getLocaisFromLista,
    getLocaisPuros,
    --getLocalPeloId,
    localToString,
    getNomeLocal,
    getRecursosList,
    getRecursosLocal,
    getRecursosLocais,
    getRecursosLocalToString,
    writeArquivoLocal) where
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
import Haskell.Agenda2 --import da agenda que criamos

data Local = Local {
    --idLocal :: Int,
    nomeLocal :: String,
    recursosLocal :: [String],
    capacidadeLocal :: Int
} deriving (Show, Read)

newtype Locais = Locais {
    locais :: [(Int, Local)]
} deriving Show

--------- Local Getters -----------
--getIdLocal :: Local -> Int
--getIdLocal Local {idLocal = i} = i

getNomeLocal :: Local -> String
getNomeLocal Local {nomeLocal = n} = n

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
    arq <- openFile "Locais.csv" AppendMode
    arq1 <- openFile "RecursosLocal.csv" AppendMode

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

getLocaisFromList :: IO [Local]
getLocaisFromList = do
    locais <- openFile "Locais.csv" ReadMode
    listaDeLocais <- lines <$> hGetContents locais
    hClose locais
    return $ convertToList listaDeLocais

convertToList :: [String] -> [Local]
convertToList [] = []
convertToList (local:list) =
    convertToLocal (split local ',') : convertToList list

convertRecursosToList :: [String] -> [[String]]
convertRecursosToList = map (`split` ',')

convertToLocal :: [String] -> Local
convertToLocal local = Local nomeLocal recursosLocal capacidadeLocal
    where
        --idLocal = read (head local) :: Int
        nomeLocal = local !! 1
        recursosLocal = fromIO (getRecursosList nomeLocal)
        capacidadeLocal = read (local !! 3) :: Int

getRecursosList :: String -> IO [String]
getRecursosList nome = do
    recursos <- openFile "RecursosLocal.csv" ReadMode
    listaDeRecursos <- lines <$> hGetContents recursos
    let recursosTotais = convertRecursosToList listaDeRecursos
    hClose recursos
    return $ filtraRecurso nome recursosTotais

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