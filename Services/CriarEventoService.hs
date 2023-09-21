module Services.CriarEventoService (criarEventoService) where

import Models.Evento (criarEvento)
import Local.Local (getLocalPorNome)
import Data.Time (Day)
import Services.AlocarHorarioService
import Models.UsuarioInstituicao
import Local.Agenda (excluirArquivoTemporario)

criarEventoService :: String -> String -> String -> String -> String -> Int -> IO ()
criarEventoService nomeEvent nomeL matriculaInst dataEvent horaEvent capacidadeEvent = do
    resultado <- criarEvento nomeEvent nomeL matriculaInst dataEvent horaEvent capacidadeEvent
    if resultado
        then do
            putStrLn "Evento criado com sucesso."
            alocarHorarioService nomeL dataEvent horaEvent matriculaInst
            excluirArquivoTemporario nomeL
        else putStrLn "O evento não foi criado devido a um erro."