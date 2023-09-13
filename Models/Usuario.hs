module Usuario (
    Usuario(Usuario),
    Usuarios(Usuarios),
    escreverArquivoUsuario,
    getUsuariosEmLista,
    getMatriculaUsuario,
    escreverAgendamentoUsuario,
    iteraAgendamentosUsuario,
    getAgendamentosMatriculaUsuario
    
) where

import Agendamento(
    getAgendamentoPeloIdLocal,
    getAgendamentos,
    Agendamentos,
    Agendamento(Agendamento)
)

import System.IO
import Utils
import System.IO.Unsafe

data Usuario = Usuario{
    nome :: String,
    matricula :: String,
    agendamentos :: [Agendamento]
} deriving (Show, Read)

data Usuarios = Usuarios{
    usuarios :: [(String, Usuario)]
} deriving Show


---- getters ----
getMatriculaUsuario :: Usuario -> String
getMatriculaUsuario (Usuario {matricula= m}) = m
getUsuariosFromTuple :: [(String, Usuario)] -> [Usuario]
getUsuariosFromTuple [] = []
getUsuariosFromTuple ((_,m): ms) = m : getUsuariosFromTuple as

getUsuariosEmLista :: Usuarios -> [Usuario]
getUsuarios (Usuarios {usuarios = u}) = getUsuariosFromTuple u

getAgendamentosUsuario :: Usuario -> [Agendamento]
getAgendamentosUsuario Usuario {agendamentos = a} = a

---- escrita ----

mapeiaMatricula :: [Usuario] -> [(String, Usuario)]
mapeia [] = []
mapeia (m:ms) = (getMatricula m, m) : mapeiaMatricula ms


adicionaAgendamentoUsuario :: Usuario -> Agendamento -> Usuario
adicionaAgendamentoUsuario user novoAgendamento =
    user { agendamentos = novoAgendamento : agendamentos user }

getAgendamentosToCsv :: [Agendamento] -> String
getAgendamentosToCsv [] = []
getAgendamentosToCsv (u:us) = if lenght agendamentoUsuario > 0 then getMatricula u ++ "," ++ getAgendamentosToString (agendamentoUsuario) ++ getAgendamentosToCsv cs 
    else []
    where
        agendamentoUsuario = getAgendamentosUsuario u

getAgendamentosToString :: [Agendamento] -> String
getAgendamentosToString [] = []

