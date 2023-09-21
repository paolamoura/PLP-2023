module Services.DesinscreverEventoService(
    desinscreverParticipanteService
) where

import Models.Evento

desinscreverParticipanteService :: String -> String -> IO()
desinscreverParticipanteService nomeEvento matriculaUsuario = do
    desinscreverParticipante nomeEvento matriculaUsuario