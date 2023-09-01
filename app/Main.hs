module Main where

import Utils.Gum
import Menu

main :: IO ()
main = do
    escolha <- gum (Choose ["Login", "Cadastrar", "Sair"] [])
    let escolhaStr = init escolha
    selecionarAcao escolha