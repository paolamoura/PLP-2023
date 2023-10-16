{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Usuario (
  Usuario(..)
  , getUsuarioByMatricula
  , cadastrarUsuario
  , confereSenha
  , getMatricula
  , removeUsuarioByMatricula
  , recordToUsuario
  , csvParseError
  , csvParseDone
  , getUsuarioCSV
  , usuarioObjToCSV
  , usuarioToCSV
  , usuarioListToCSV
  , saveUsuarioCSV
  , editUsuarioSenhaCSV
  , removeUsuarioCSV) where

import System.IO.Unsafe
import Text.CSV
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import System.IO
import System.Directory
import qualified Data.Vector as V
import Data.Csv (FromRecord(..), ToRecord(..), HasHeader(..), decode)

data Usuario = Usuario
    { matricula :: String
    , nome :: String
    , senha :: String
    } deriving (Show, Generic)

instance FromRecord Usuario
instance ToRecord Usuario

-- Define o caminho do arquivo CSV
csvFilePath :: FilePath
csvFilePath = "./Arquivos/Usuarios.csv"

cadastrarUsuario :: String -> String -> String -> IO (Maybe Usuario)
cadastrarUsuario matricula nome senha = do
    -- Verifica se já existe um usuário com a mesma matrícula
    usuarioExistente <- getUsuarioByMatricula matricula
    case usuarioExistente of
        Just _ -> do
            putStrLn "Já existe um usuário com essa matrícula."
            return Nothing
        Nothing -> do
            let novoUsuario = Usuario matricula nome senha
            saveUsuarioCSV matricula nome senha
            return (Just novoUsuario)

-- Função para carregar todos os usuários do arquivo CSV
carregarUsuarios :: IO [Usuario]
carregarUsuarios = do
    csvData <- B.readFile csvFilePath
    case decode NoHeader csvData of
        Left err -> do
            putStrLn $ "Erro ao ler o arquivo CSV: " ++ err
            return []  -- Retorna uma lista vazia em caso de erro
        Right usuarios -> return (V.toList usuarios)  -- Converte para lista

-- Função para encontrar um usuário pelo número de matrícula
getUsuarioByMatricula :: String -> IO (Maybe Usuario)
getUsuarioByMatricula matriculaBuscada = do
    usuarios <- carregarUsuarios
    let usuarioEncontrado = filter (\u -> matricula u == matriculaBuscada) usuarios
    return $ case usuarioEncontrado of
        [u] -> Just u
        _ -> Nothing

-- Função para verificar se a senha de um usuário corresponde à senha fornecida
confereSenha :: Usuario -> String -> Bool
confereSenha usuario senhaFornecida = senha usuario == senhaFornecida

removeUsuarioByMatricula :: String -> [Usuario] -> [Usuario]
removeUsuarioByMatricula _ [] = []
removeUsuarioByMatricula matriculaS (x:xs)
  | matricula x == matriculaS = xs
  | otherwise = [x] ++ removeUsuarioByMatricula matriculaS xs

recordToUsuario :: [Record] -> [Usuario]
recordToUsuario [] = []
recordToUsuario (x:xs) = do
  let u = Usuario (x !! 0) (x !! 1) (x !! 2)
  [u] ++ recordToUsuario xs

-- Altere a assinatura da função csvParseError
csvParseError :: String -> [Record]
csvParseError _ = []

-- Altere a assinatura da função csvParseDone
csvParseDone :: Either String CSV -> [Record]
csvParseDone (Right csvFile) = tail csvFile
csvParseDone (Left _) = []  -- Lida com o erro, retornando uma lista vazia

getUsuarioCSV :: FilePath -> IO (Either String CSV)
getUsuarioCSV path = do
  file <- readFile path
  let parsedCSV = parseCSV path file
  case parsedCSV of
    Left err -> return (Left $ "Erro ao fazer parsing do arquivo CSV: " ++ show err)
    Right csvFile -> return (Right csvFile)

usuarioObjToCSV :: Usuario -> String
usuarioObjToCSV u = "\n" ++ matricula u ++ "," ++ nome u ++ "," ++ senha u

usuarioToCSV :: String -> String -> String -> String
usuarioToCSV matricula nome senha = "\n" ++ matricula ++ "," ++ nome ++ "," ++ senha

usuarioListToCSV :: [Usuario] -> String
usuarioListToCSV [] = ""
usuarioListToCSV (x:xs) = usuarioObjToCSV x ++ usuarioListToCSV xs

saveUsuarioCSV :: String -> String -> String -> IO ()
saveUsuarioCSV matricula nome senha = do
  let u = usuarioToCSV matricula nome senha
  f <- openFile csvFilePath AppendMode
  hPutStr f u
  hClose f

editUsuarioSenhaCSV :: FilePath -> String -> String -> String -> IO (Either String ())
editUsuarioSenhaCSV csvFilePath matricula nome senha = do
  -- Lê o arquivo CSV
  csvContents <- getUsuarioCSV csvFilePath
  case csvContents of
    Left err -> return (Left err)  -- Retorna o erro se a leitura falhar
    Right csvFile -> do
      let usuarioList = recordToUsuario csvFile
      let u = Usuario matricula nome senha
      let newUsuarioList = removeUsuarioByMatricula matricula usuarioList ++ [u]
      let usuarioListCSV = "matricula,nome,senha" ++ usuarioListToCSV newUsuarioList
      -- Escreve os dados atualizados de volta no arquivo
      B.writeFile "../Temp.csv" $ BC.pack usuarioListCSV
      removeFile csvFilePath
      renameFile "../Temp.csv" csvFilePath
      return (Right ())

removeUsuarioCSV :: FilePath -> String -> IO (Either String ())
removeUsuarioCSV csvFilePath matricula = do
  -- Lê o arquivo CSV
  csvContents <- getUsuarioCSV csvFilePath
  case csvContents of
    Left err -> return (Left err)  -- Retorna o erro se a leitura falhar
    Right csvFile -> do
      let usuarioList = recordToUsuario csvFile
      let newUsuarioList = removeUsuarioByMatricula matricula usuarioList
      let usuarioListCSV = "matricula,nome,senha" ++ usuarioListToCSV newUsuarioList
      -- Escreve os dados atualizados de volta no arquivo
      B.writeFile "../Temp.csv" $ BC.pack usuarioListCSV
      removeFile csvFilePath
      renameFile "../Temp.csv" csvFilePath
      return (Right ())

-- Função para obter a matrícula de um usuário
getMatricula :: Usuario -> String
getMatricula = matricula