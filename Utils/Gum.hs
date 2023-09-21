module Utils.Gum where

import System.Process
import System.IO
import Data.Char (isSpace)
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

data GumFlag
    = Flag String
    | FlagWithArg String String
    deriving Show

data GumCommand
    = Choose [String] [GumFlag]
    | Confirm String [GumFlag]
    | Filter [String] [GumFlag]
    | Format String [String] [GumFlag]
    | Input [GumFlag]
    | Join String [String] [GumFlag]
    | Style [GumFlag]
    | Write [GumFlag]

gum :: GumCommand -> IO String
gum command = do
    output <- case command of
        Choose options flags -> readProcess "gum" (constructArgs "choose" options flags) ""
        Confirm message flags -> readProcess "gum" (constructArgs "confirm" [message] flags) ""
        Filter items flags -> readProcess "gum" (constructArgs "filter" items flags) ""
        Format template args flags -> readProcess "gum" (constructArgs "format" (template : args) flags) ""
        Input flags -> readProcess "gum" (constructArgs "input" [] flags) ""
        Join delimiter items flags -> readProcess "gum" (constructArgs "join" (delimiter : items) flags) ""
        Style flags -> readProcess "gum" (constructArgs "style" [] flags) ""
        Write flags -> readProcess "gum" (constructArgs "write" [] flags) ""

    return (trimOutput output)

-- Função para fazer o trim do output
trimOutput :: String -> String
trimOutput = reverse . dropWhile isSpace . reverse . dropWhile isSpace

constructArgs :: String -> [String] -> [GumFlag] -> [String]
constructArgs cmd options flags =
    cmd : concatMap flagToArgs flags ++ filter (not . null) options

flagToArgs :: GumFlag -> [String]
flagToArgs (Flag flagName) = [flagName]
flagToArgs (FlagWithArg flagName arg) = [flagName, arg]

-- Função para executar um comando com entrada redirecionada e saída para o terminal atual.
runCommandWithInput :: String -> String -> IO String
runCommandWithInput cmd input = do
    (stdin, stdout, _, processHandle) <- createProcess (shell cmd) {
        std_in = CreatePipe,
        std_out = CreatePipe,
        std_err = Inherit
    }
    hPutStr (fromJust stdin) input
    hClose (fromJust stdin)
    output <- hGetContents (fromJust stdout)
    _ <- waitForProcess processHandle
    return output

-- Função para executar o comando Gum Table com um arquivo de caminho especificado.
gumTable :: String -> [GumFlag] -> IO String
gumTable filePath flags = do
    let cmd = "gum " ++ unwords (constructArgs "table" [] flags) ++ " < "  ++ filePath
    runCommandWithInput cmd ""

data DadosCSV = DadosCSV
    { dataCampo :: String
    , horaCampo :: String
    , disponibilidadeCampo :: String
    , responsavelCampo :: String
    , listaEsperaCampo :: String
    } deriving (Show)

-- Função para extrair os dados da linha do CSV
extrairDados :: BL.ByteString -> Maybe DadosCSV
extrairDados csvData = do
    let decoded = decode NoHeader csvData :: Either String (V.Vector String)
    case decoded of
        Left err -> do
            Nothing
        Right row -> do
            case V.toList row of
                [dataStr, horaStr, disponibilidadeStr, responsavelStr, listaEsperaStr] ->
                    Just $ DadosCSV dataStr horaStr disponibilidadeStr responsavelStr listaEsperaStr
                _ -> do
                    Nothing

-- Função para converter Either em Maybe
eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left x) = Just x
eitherToMaybe (Right _) = Nothing