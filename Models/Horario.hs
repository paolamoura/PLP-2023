module Models.Horario where

import Data.List (sortBy)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import System.Directory (doesFileExist)

import Models.Usuario (Usuario, carregarUsuariosDeCSV, atualizarUsuarioCSV)

data Local = Local
    { nomeLocal :: String
    -- Outros atributos do local, se necessário
    } deriving (Show, Eq)

data Horario = Horario
    { espaco :: Local
    , horario :: (String, String)
    , pessoaComHorario :: Maybe Usuario
    , listaEspera :: [(Usuario, String)] -- Uma lista de espera ordenada pela matrícula como String
    } deriving (Show, Eq)

instance ToNamedRecord Horario
instance FromNamedRecord Horario
instance DefaultOrdered Horario

-- Função para criar um novo horário
criarHorario :: Local -> (String,String) -> Horario
criarHorario local horario = Horario local horario Nothing []

-- Função para alocar um horário a um usuário
alocarHorario :: Usuario -> Horario -> FilePath -> IO (Maybe Usuario)
alocarHorario usuario horario filePath = do
    usuarios <- carregarUsuariosDeCSV filePath
    if length (horariosUsuario usuario) >= 3
        then return Nothing
        else do
            let usuarioAtualizado = usuario { horariosUsuario = horario : horariosUsuario usuario }
            atualizarUsuarioCSV usuarioAtualizado usuarios filePath
            return (Just usuarioAtualizado)

-- Função para desalocar um usuário de um horário e verificar o próximo da lista de espera
desalocarUsuario :: Horario -> Usuario -> FilePath -> IO (Maybe Usuario)
desalocarUsuario horario usuario filePath = do
    usuarios <- carregarUsuariosDeCSV filePath
    let horariosAtualizados = filter (/= horario) (horariosUsuario usuario)
    let usuarioAtualizado = usuario { horariosUsuario = horariosAtualizados }
    if null (listaEspera horario)
        then do
            atualizarUsuarioCSV usuarioAtualizado usuarios filePath
            return (Just usuarioAtualizado)
        else do
            let (proxUser, _) = head (listaEspera horario)
            if length (horariosUsuario proxUser) < 3
                then do
                    let proxUserAtualizado = proxUser { horariosUsuario = horario : horariosUsuario proxUser }
                    let listaEsperaAtualizada = tail (listaEspera horario)
                    let horarioAtualizado = horario { listaEspera = listaEsperaAtualizada }
                    atualizarUsuarioCSV usuarioAtualizado usuarios filePath
                    atualizarUsuarioCSV proxUserAtualizado usuarios filePath
                    return (Just proxUserAtualizado)
                else do
                    let listaEsperaAtualizada = tail (listaEspera horario)
                    let horarioAtualizado = horario { listaEspera = listaEsperaAtualizada }
                    atualizarUsuarioCSV usuarioAtualizado usuarios filePath
                    return (Just usuarioAtualizado)

-- Função para adicionar um usuário à lista de espera
adicionarUsuarioListaEspera :: [(Usuario, String)] -> Usuario -> [(Usuario, String)]
adicionarUsuarioListaEspera listaEspera usuario =
    let listaAtualizada = (usuario, matriculaUsuario usuario) : listaEspera
        listaOrdenada = sortBy (\(_, matricula1) (_, matricula2) -> compare matricula1 matricula2) listaAtualizada
    in listaOrdenada

-- Função para escrever horários em um arquivo CSV
escreverHorariosEmCSV :: [Horario] -> FilePath -> IO ()
escreverHorariosEmCSV horarios filePath = do
    let csvData = encodeDefaultOrderedByName horarios
    BL.writeFile filePath csvData

-- Função para ler horários de um arquivo CSV
lerHorariosDeCSV :: FilePath -> IO [Horario]
lerHorariosDeCSV filePath = do
    arquivoExiste <- doesFileExist filePath
    if arquivoExiste
        then do
            csvData <- BL.readFile filePath
            case decode NoHeader csvData of
                Left _ -> return []
                Right horarios -> return horarios
        else return []
