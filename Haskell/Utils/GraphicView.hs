module Utils.GraphicView where

import System.Process
import System.IO (hPutStrLn, hClose)
import Services.EstatisticaService (somaInteresses, somaInteressesTotal)

plotVerticalBarChart :: [(String, Int)] -> IO ()
plotVerticalBarChart dataPoints = do
    let plotData = unlines $ map (\(x, y) -> x ++ " " ++ show y) dataPoints
    let gnuplotScript =
            "set term dumb; set boxwidth 0.8 relative; set style fill solid; " ++
            "plot '-' using 2:xtic(1) with boxes notitle\n" ++
            plotData ++
            "e\n"
    
    (Just stdinH, _, _, processHandle) <- createProcess (shell "gnuplot") {
        std_in = CreatePipe,
        std_out = Inherit,
        std_err = Inherit
    }
    
    hPutStrLn stdinH gnuplotScript
    hClose stdinH
    
    _ <- waitForProcess processHandle
    
    return ()

printarGraficoGeral :: IO ()
printarGraficoGeral = do
    dataPoints <- somaInteressesTotal
    plotVerticalBarChart dataPoints

printarGraficoLocal :: String -> IO ()
printarGraficoLocal nomeLocal = do
    dataPoints <- somaInteresses nomeLocal 
    plotVerticalBarChart dataPoints