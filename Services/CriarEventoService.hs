module Services.CriarEventoService (criarEventoService) where

import Models.Evento (criarEvento)
import Local.Local (getLocalPorNome)
import Data.Time (Day)
import Services.AlocarHorarioService
import Models.UsuarioInstituicao

criarEventoService :: String -> String -> String -> Day -> String -> Int -> IO ()
criarEventoService nomeEvent matriculaInst nomeL dataEvent horaEvent capacidadeEvent = do
    maybeUsuario <- getUsuarioByMatricula matriculaInst
    case maybeUsuario of
        Just inst -> do
            resultado <- criarEvento nomeEvent nomeL (nome inst) dataEvent horaEvent capacidadeEvent
            if resultado
                then do
                    putStrLn "Evento criado com sucesso."
                    alocarHorarioService nomeL (show dataEvent) horaEvent matriculaInst
                else putStrLn "O evento não foi criado devido a um erro."
        Nothing -> putStrLn "Usuário não encontrado, o evento não será criado."
