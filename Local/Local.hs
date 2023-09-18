module Local.Local (Local(Local), Locais(Locais),
    getIdLocal,
    getCapacidadeLocal,
    getLocais,
    getLocaisFromList,
    getLocaisFromLista,
    getLocaisPuros,
    getLocalPeloId,
    localToString,
    getNomeLocal,
    getRecursosList,
    getRecursosLocal,
    getRecursosLocais,
    getRecursosLocalToString) where
import System.IO
import System.Directory
import Haskell.Util
import System.IO.Unsafe (unsafePerformIO)

import Control.Exception
import System.IO.Error
import System.Process
import Control.Monad (when)
import Text.Printf

data Local = Local {
    idLocal :: Int,
    nomeLocal :: String,
    recursosLocal :: [String],
    capacidadeLocal :: Int
} deriving (Show, Read)

newtype Locais = Locais {
    locais :: [(Int, Local)]
} deriving Show

--------- Local Getters -----------
getIdLocal :: Local -> Int
getIdLocal Local {idLocal = i} = i

getNomeLocal :: Local -> String
getNomeLocal Local {nomeLocal = n} = n

getRecursosLocal :: Local -> [String]
getRecursosLocal Local {recursosLocal = r} = r

getRecursosLocais :: [Local] -> String
getRecursosLocais [] = []
getRecursosLocais (l:ls) = show (getIdLocal l) ++ "," ++ getRecursosLocalToString (getRecursosLocal l) ++ getRecursosLocais ls

getCapacidadeLocal :: Local -> Int
getCapacidadeLocal Local {capacidadeLocal = c} = c

getLocalPeloId :: Int -> [Local] -> Maybe Local
getLocalPeloId id [] = Nothing
getLocalPeloId id (l:ls) = if id == getIdLocal l then Just l
                        else getLocalPeloId id ls

getLocaisFromLista :: [(Int, Local)] -> [Local]
getLocaisFromLista [] = []
getLocaisFromLista ((_,l): ls) = l : getLocaisFromLista ls

getLocais :: Locais -> [Local]
getLocais (Locais {locais = l}) = getLocaisFromLista l

getRecursosLocalToString :: [String] -> String
getRecursosLocalToString [] = []
getRecursosLocalToString (r:rs) = if not (null rs) then r ++ "," ++ getRecursosLocalToString rs else r ++ "\n"

localToString :: Local -> String
localToString Local {idLocal = i, nomeLocal = n, recursosLocal = r, capacidadeLocal = c} =  "Id: " ++ show i ++"\n" ++
                                                                                            "Nome: " ++ n ++ "\n" ++
                                                                                            "Recursos: " ++ getRecursosLocalToString r  ++
                                                                                            "Capacidade: " ++ show c

--------- Escrever Arquivo Local para Usuário ------------
writeArquivoLocal :: [Local] -> IO()
writeArquivoLocal local = do
    arq <- openFile "../Arquivos/Locais.csv" AppendMode
    arq1 <- openFile "../Arquivos/RecursosLocal.csv" AppendMode

    print local -- mudar isso, mas fazer depois

    let dataRecursosLocal = getRecursosLocais local
    hPutStr arq1 dataRecursosLocal
    hPutStr arq (toWrite local)
    hClose arq
    hClose arq1

toWrite :: [Local] -> String
toWrite [] = []
toWrite (x:xs) = localToString x ++ "\n" ++ toWrite xs

--------- Visualizar Local ------------
getLocaisPuros :: [Local]
getLocaisPuros = unsafePerformIO getLocaisFromList :: [Local]

getLocaisFromList :: IO [Local]
getLocaisFromList = do
    locais <- openFile "../Arquivos/Locais.csv" ReadMode
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
convertToLocal local = Local idLocal nomeLocal recursosLocal capacidadeLocal
    where
        idLocal = read (head local) :: Int
        nomeLocal = local !! 1
        recursosLocal = fromIO (getRecursosList idLocal)
        capacidadeLocal = read (local !! 3) :: Int

getRecursosList :: Int -> IO [String]
getRecursosList id = do
    recursos <- openFile "../Arquivos/RecursosLocal.csv" ReadMode
    listaDeRecursos <- lines <$> hGetContents recursos
    let recursosTotais = convertRecursosToList listaDeRecursos
    hClose recursos
    return $ filtraRecurso id recursosTotais

filtraRecurso :: Int -> [[String]] -> [String]
filtraRecurso _ [] = []
filtraRecurso id (recurso:recursos)
    | id == idLocal = tail recurso
    | otherwise = filtraRecurso id recursos
    where
        idLocal = read (head recurso) :: Int

---- Retirar impurezas do IO ----
fromIO :: IO[String] -> [String]
fromIO x = unsafePerformIO x :: [String]