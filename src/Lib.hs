module Lib
    ( someFunc
    ) where

import Data.Attoparsec.Char8
import Control.Applicative ((<|>))

csv-file :: Parser [Row]
csv-file = many row <* endOfLine


row :: Parser Row
row      = do 
           fn <-  first_name
	   char ','
           ln <- last_name
           char ','
           e <- email
           char ','
           g <- grade
	   return (Row fn ln e g)

Data Row = Row FirstName LastName Email Grade

type FirstName = String
type LastName  = String
type Email     = String
type Grade     = String




first_name :: Parser FirtsName
first_name = many letter_ascii

last_name :: Parser LastName
last_name = many letter_ascii

email :: Parser Email
email = do 
        vorDemNamen <- takeTill (== '@') 
        char '@'
        nachDemNamen <- many anyChar
        return (vorDemNamen ++ "@" ++ nachDemNamen) 
        

grade :: Parser Grade
grade = many digit

float :: Parser Float
float = do 
	vorKomma <- takeTill (== '.')
        char '.'
        nachKomma <- many digit
	let floatx = vorKomma ++ "." ++ nachKomma
        return (read floatx :: Float)  

someFunc :: IO ()
someFunc = putStrLn "someFunc" 
