module Services.CriarLocalService (
    criarLocalService
) where

import Local.Local (criarLocal)
import Local.Agenda (geraAgenda)

criarLocalService :: String -> [String] -> Int -> IO ()
criarLocalService nome recursos capacidade = do
    criarLocal nome recursos capacidade
    geraAgenda nome