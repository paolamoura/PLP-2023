module Login.CadastraLogin where
import Models.Usuario(criarUsuario)
import Data.Char (isDigit)

data UsuarioLogin = UsuarioLogin
    { nome :: String
    , matricula :: String
    , senha :: String
    } deriving (Show)

-- Função para validar a matrícula
validarMatricula :: String -> Bool
validarMatricula matricula =
validarMatricula :: String -> Bool
validarMatricula matricula =
    length matricula == 9 && -- A matrícula deve ter tamanho 9
    all isDigit matricula && -- A matrícula deve conter apenas dígitos
    let digito2 = read [matricula !! 1, matricula !! 2] :: Int
        digito4 = read [matricula !! 3] :: Int
    in digito4 == 1 || digito4 == 2 && digito2 >= 17 && digito2 <= 23

-- Função para cadastrar um usuário
cadastrarUsuario :: String -> String -> String -> String -> Maybe Usuario
cadastrarUsuario nome matricula senha confirmacaoSenha
    | senha /= confirmacaoSenha = Nothing -- Senha e confirmação de senha não coincidem
    | not (validarMatricula matricula) = Nothing -- Matrícula inválida
    | otherwise = Just (criarUsuario matricula nome senha) 

-- Função para autenticar um usuário
autenticarUsuario :: String -> String -> [Usuario] -> Maybe Usuario
autenticarUsuario matricula senha usuarios =
    case filter (\u -> matricula == matricula u && senha == senha u) usuarios of
        [usuario] -> Just usuario -> Nothing