module Services.InscreverEventoService(
    inscreverParticipanteService
) where

import Models.Evento

inscreverParticipanteService :: String -> String -> IO()
inscreverParticipanteService nomeEvento matriculaUsuario = do
    inscreverParticipante nomeEvento matriculaUsuario