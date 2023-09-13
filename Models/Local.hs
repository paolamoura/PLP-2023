module Models.Local (Local, criarLocal, getNomeLocal, getTipoDeAlocacao, setTipoDeAlocacao, getLocais, saveLocais) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import qualified Data.Csv as Csv
import Data.Csv ((.:), (.=))
import qualified Data.Vector as V
import Data.Text (unpack, pack)
import Data.Text.IO (readFile, writeFile)

data Local = Local {
    nomeLocal :: String,
    tipoDeAlocacao :: String
}

-- Função para criar um local
criarLocal :: String -> String -> Local
criarLocal nome tipo = Local nome tipo

-- Função para obter o nome de um local
getNomeLocal :: Local -> String
getNomeLocal = nomeLocal

-- Função para obter o tipo de alocação de um local
getTipoDeAlocacao :: Local -> String
getTipoDeAlocacao = tipoDeAlocacao

-- Função para definir o tipo de alocação de um local
setTipoDeAlocacao :: String -> Local -> Local
setTipoDeAlocacao tipo local = local { tipoDeAlocacao = tipo }

-- Define o diretório e o arquivo onde os dados serão armazenados
dataDir :: FilePath
dataDir = "Arquivos"

csvFile :: FilePath
csvFile = dataDir </> "Locais.csv"

-- Função para criar o diretório de dados, se não existir
createDataDir :: IO ()
createDataDir = createDirectoryIfMissing True dataDir

-- Função para carregar os locais do arquivo CSV
getLocais :: IO [Local]
getLocais = do
    createDataDir
    fileExists <- doesFileExist csvFile
    if fileExists
        then do
            csvData <- Data.Text.IO.readFile csvFile
            case Csv.decode Csv.HasHeader csvData of
                Left err -> do
                    putStrLn ("Erro ao carregar o arquivo CSV: " ++ err)
                    return []
                Right locais -> return locais
        else return []

-- Função para salvar os locais no arquivo CSV
saveLocais :: [Local] -> IO ()
saveLocais locais = do
    createDataDir
    Data.Text.IO.writeFile csvFile (Csv.encodeDefaultOrderedByName (map toCsvRow locais))
  where
    toCsvRow local = Csv.namedRecord [
        "NomeLocal" .= pack (getNomeLocal local),
        "TipoDeAlocacao" .= pack (getTipoDeAlocacao local)
    ]
