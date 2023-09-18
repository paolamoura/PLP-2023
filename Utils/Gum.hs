module Utils.Gum where

import System.Process
import Data.Char (isSpace)

data GumFlag
    = Flag String
    | FlagWithArg String String
    deriving Show

data GumCommand
    = Choose [String] [GumFlag]
    | Confirm String [GumFlag]
    | Filter [String] [GumFlag]
    | Format String [String] [GumFlag]
    | Input [GumFlag]
    | Join String [String] [GumFlag]
    | Style [GumFlag]
    | Write [GumFlag]

gum :: GumCommand -> IO String
gum command = do
    output <- case command of
        Choose options flags -> readProcess "gum" (constructArgs "choose" options flags) ""
        Confirm message flags -> readProcess "gum" (constructArgs "confirm" [message] flags) ""
        Filter items flags -> readProcess "gum" (constructArgs "filter" items flags) ""
        Format template args flags -> readProcess "gum" (constructArgs "format" (template : args) flags) ""
        Input flags -> readProcess "gum" (constructArgs "input" [] flags) ""
        Join delimiter items flags -> readProcess "gum" (constructArgs "join" (delimiter : items) flags) ""
        Style flags -> readProcess "gum" (constructArgs "style" [] flags) ""
        Write flags -> readProcess "gum" (constructArgs "write" [] flags) ""

    return (trimOutput output)

-- Função para fazer o trim do output
trimOutput :: String -> String
trimOutput = reverse . dropWhile isSpace . reverse . dropWhile isSpace

constructArgs :: String -> [String] -> [GumFlag] -> [String]
constructArgs cmd options flags =
    cmd : concatMap flagToArgs flags ++ filter (not . null) options

flagToArgs :: GumFlag -> [String]
flagToArgs (Flag flagName) = [flagName]
flagToArgs (FlagWithArg flagName arg) = [flagName, arg]
