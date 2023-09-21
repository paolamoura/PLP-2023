module Services.EstatisticaService (
    somaInteresses,
    somaInteressesTotal
) where

import Local.Local
import Local.Agenda (contagemDiasDaSemana)
import Control.Monad (foldM)

-- Função para somar duas listas elemento a elemento
somaListas :: [Int] -> [Int] -> [Int]
somaListas [] [] = []
somaListas (x:xs) (y:ys) = (x + y) : somaListas xs ys
somaListas _ _ = error "As listas devem ter o mesmo tamanho"

-- Função para calcular a soma total das contagens de interesse por dia
somaInteressesTotal :: IO [(String, Int)]
somaInteressesTotal = do
    nomesLocais <- getNomesLocaisCSV
    let diasDaSemana = ["Segunda", "Terca", "Quarta", "Quinta", "Sexta"]
    somaTotal <- foldM (\soma nomeLocal -> do
        contagemLocal <- contagemDiasDaSemana nomeLocal
        let somaLocal = somaListas soma contagemLocal
        return somaLocal)
        (replicate 5 0) nomesLocais
    return $ zip diasDaSemana somaTotal

somaInteresses :: String -> IO [(String, Int)]
somaInteresses nomeLocal = do
    contagemLocal <- contagemDiasDaSemana nomeLocal
    let diasDaSemana = ["Segunda", "Terca", "Quarta", "Quinta", "Sexta"]
    return $ zip diasDaSemana contagemLocal