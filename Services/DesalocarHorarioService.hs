module Services.DesalocarHorarioService(
    desalocaHorarioService
) where

import Local.Agenda(desaloca)

desalocaHorarioService :: String -> String -> String -> String -> IO()
desalocaHorarioService nomeLocal dia hora responsavel = do
    desaloca nomeLocal dia hora responsavel