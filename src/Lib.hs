{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( Row
      , csvFile
    ) where

import Data.Attoparsec.Text
import Control.Applicative ((<|>))
import Data.Monoid
import Data.Text
import Debug.Trace

csvFile :: Parser [Row]
csvFile = many' row
--row = field-list, eol
--field-list = field [ ",", field-list]
--field = [whitespace]

--eol = "\n"

row :: Parser Row
row      = do
           fn <-  first_name
           char ','
           ln <-  last_name
           char ','
           e <-  email
           char ','
           g <-  grade
           let r = Row fn ln e g
           endOfLine <|> endOfInput
           return r

data Row = Row FirstName LastName Email Grade
            deriving (Show, Eq)

type FirstName = Text
type LastName  = Text
type Email     = Text
type Grade     = Text

first_name :: Parser FirstName
first_name = takeTill (== ',')  <?> "FirstName" 

last_name :: Parser LastName
last_name = takeTill (== ',') <?> "LastName"

email :: Parser Email
email= takeTill (== ',') <?> "email"

grade :: Parser Grade
grade = takeTill isEndOfLine <?> "grade"
