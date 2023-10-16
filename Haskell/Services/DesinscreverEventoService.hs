module Services.DesinscreverEventoService(
    desinscreverParticipanteService
) where

import Models.Evento

desinscreverParticipanteService :: String -> String -> IO Bool
desinscreverParticipanteService nomeEvento matriculaUsuario = do
    desinscreverParticipante nomeEvento matriculaUsuario