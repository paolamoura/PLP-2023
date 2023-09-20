module Services.AlocarHorarioService(
    alocarHorarioService
) where

import Local.Agenda(aloca)

alocarHorarioService :: String -> String -> String -> String -> IO ()
alocarHorarioService nomeLocal dia hora responsavel = do
    aloca nomeLocal dia hora responsavel