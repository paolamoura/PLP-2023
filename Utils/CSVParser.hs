module Utils.CSVParser where

import Data.List.Split (splitOn)

data CSVRow = CSVRow [String] deriving (Show)

parseCSV :: String -> [CSVRow]
parseCSV csvString = map parseRow (lines csvString)
  where
    parseRow :: String -> CSVRow
    parseRow line = CSVRow (splitOn "," line)
