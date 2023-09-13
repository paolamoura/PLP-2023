module Models.Usuario where

import Data.Csv
import qualified Data.Vector as Data.Vector
import qualified Data.ByteString.Lazy as BL
import Models.Evento

data Usuario = Usuario
    { matriculaUsuario :: String
    , nomeUsuario :: String
    , senhaUsuario :: String
    , eventosUsuario :: [Evento]
    } deriving (Show, Eq)

instance ToNamedRecord Usuario
instance FromNamedRecord Usuario
instance DefaultOrdered Usuario

-- Função para criar um novo usuário com matrícula única
criarUsuario :: String -> String -> String -> FilePath -> IO (Maybe Usuario)
criarUsuario matricula nome senha filePath = do
    let filePath = "Arquivos/Usuarios.csv"
    matriculasExistentes <- lerMatriculasDeCSV filePath
    if matricula `elem` matriculasExistentes
        then return Nothing
        else do
            let novoUsuario = Usuario matricula nome senha []
            salvarUsuarioCSV novoUsuario filePath
            return (Just novoUsuario)

-- Função para atualizar um usuário no arquivo CSV
atualizarUsuarioCSV :: Usuario -> [Usuario] -> FilePath -> IO ()
atualizarUsuarioCSV usuario usuarios filePath = do
    let filePath = "Arquivos/Usuarios.csv"
    let usuariosAtualizados = map (\u -> if matriculaUsuario u == matriculaUsuario usuario
                                            then usuario
                                            else u) usuarios
    salvarUsuarioCSV (head usuariosAtualizados) filePath

-- Função para inscrever em evento
inscreverEmEvento :: Usuario -> Evento -> FilePath -> IO (Maybe Usuario)
inscreverEmEvento usuario evento filePath = do
    let filePath = "Arquivos/Usuarios.csv"
    usuarios <- carregarUsuariosDeCSV filePath
    let usuarioAtualizado = usuario { eventosUsuario = evento : eventosUsuario usuario }
    atualizarUsuarioCSV usuarioAtualizado usuarios filePath
    return (Just usuarioAtualizado)

-- Função para desinscrever de evento
desinscreverDeEvento :: Usuario -> Evento -> FilePath -> IO (Maybe Usuario)
desinscreverDeEvento usuario evento filePath = do
    let filePath = "Arquivos/Usuarios.csv"
    usuarios <- carregarUsuariosDeCSV filePath
    let eventosAtualizados = filter (/= evento) (eventosUsuario usuario)
    let usuarioAtualizado = usuario { eventosUsuario = eventosAtualizados }
    atualizarUsuarioCSV usuarioAtualizado usuarios filePath
    return (Just usuarioAtualizado)

-- Função para salvar um usuário no arquivo CSV
salvarUsuarioCSV :: Usuario -> FilePath -> IO ()
salvarUsuarioCSV usuario filePath = do
    let filePath = "Arquivos/Usuarios.csv"
    let csvData = encodeDefaultOrderedByName [usuario]
    BL.writeFile filePath csvData
-- Função para carregar usuários do arquivo CSV
-- Função para carregar usuários do arquivo CSV
carregarUsuariosDeCSV :: FilePath -> IO [Usuario]
carregarUsuariosDeCSV filePath = do
    let filePath = "Arquivos/Usuarios.csv"
    csvData <- BL.readFile filePath
    case decode NoHeader csvData of
        Left _ -> return []
        Right usuarios -> return (Data.Vector.toList usuarios)

-- Função para ler as matrículas de usuários a partir de um arquivo CSV
lerMatriculasDeCSV :: FilePath -> IO [String]
lerMatriculasDeCSV filePath = do
    let filePath = "Arquivos/Usuarios.csv"
    usuarios <- carregarUsuariosDeCSV filePath
    return (map matriculaUsuario usuarios)
