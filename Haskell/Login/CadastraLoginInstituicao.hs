module Login.CadastraLoginInstituicao where

import System.FilePath
import Control.Monad (liftM)
import Models.UsuarioInstituicao(UsuarioInstituicao, cadastrarUsuario, getUsuarioByMatricula, confereSenha)
import Data.Char (isDigit)

data UsuarioLogin = UsuarioLogin
    { nome :: String
    , matricula :: String
    , senha :: String
    } deriving (Show)

-- Função para validar a matrícula
validarMatricula :: String -> Bool
validarMatricula matricula =
    length matricula == 9 && -- A matrícula deve ter tamanho 9
     -- A matrícula deve ter tamanho 9
    all isDigit matricula && -- A matrícula deve conter apenas dígitos
    let digito2 = read [matricula !! 1, matricula !! 2] :: Int
        digito4 = read [matricula !! 3] :: Int
        digito5 = read [head matricula] :: Int
    in digito4 == 1 || digito4 == 2 && digito2 >= 17 && digito2 <= 23 && digito5 == 0

fazerCadastro :: String -> String -> String -> String -> IO (Maybe UsuarioInstituicao)
fazerCadastro matricula username senha confirmacaoSenha
    | senha /= confirmacaoSenha = return Nothing
    | not (validarMatricula matricula) = return Nothing
    | otherwise = do
        usuario <- cadastrarUsuario matricula username senha
        return usuario

fazerLogin :: String -> String -> IO (Maybe UsuarioInstituicao)
fazerLogin matricula senha = do
    maybeUsuario <- getUsuarioByMatricula matricula
    case maybeUsuario of
        Just usuario ->
            if confereSenha usuario senha
                then return (Just usuario)
                else return Nothing
        Nothing -> return Nothing