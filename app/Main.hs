module Main where

import Lib
import Data.Attoparsec.Text
import Data.Text

main :: IO ()
main = do
        f <- readFile "CSV_Dummy_simple.csv"
        print $ parseOnly csvFile (pack f)
