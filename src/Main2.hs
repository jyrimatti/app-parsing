{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, GADTs #-}
module Main2 where

import Prelude (String,($),(/=),(<$>),(.),Show,show,putStrLn)
import Control.Applicative (many,(<|>),(*>),(<*))

-- import a parsing library supporting applicative parsing,
-- and your existing definitions work pretty much out of the box!

import Control.Applicative.Combinators (between)
import Text.Megaparsec (parse)
import Text.Megaparsec.String (Parser)
import Text.Megaparsec.Char (char,space,satisfy,string)

data Term = Str String
          | Concat [Term]
          | Print Term deriving Show

-- needs a single type signature
term :: Parser Term
term = space *> (str <|> inparens (concat <|> print))

str    = Str    <$> (char '"' *> many (notChar '"') <* char '"')
concat = Concat <$> (char '+' *> many term)
print  = Print  <$> (string "print" *> term)

inparens = between (char '(') (char ')')

-- included in megaparsec 6.x
notChar = satisfy . (/=)

-- (print (+ "Hello " "World!")
main2 = putStrLn $ show $ parse term "" "(print (+ \"Hello \" \"World!\"))"