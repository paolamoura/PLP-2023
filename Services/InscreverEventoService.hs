module Services.InscreverEventoService(
    inscreverParticipanteService
) where

import Models.Evento

inscreverParticipanteService :: String -> String -> IO Bool
inscreverParticipanteService nomeEvento matriculaUsuario = do
    inscreverParticipante nomeEvento matriculaUsuario