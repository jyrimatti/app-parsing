{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, DeriveFunctor #-}
module Main where

import Prelude (String,($),(.),(==),(/=),Show,show,putStrLn)
import Data.Functor (Functor,(<$>))
import Data.Traversable (traverse)
import Control.Applicative (Applicative(pure),Alternative(empty),many,(<|>),(<*>),(*>),(<*))
import Text.Megaparsec.String (Parser)
import Text.Megaparsec (parse)
import Text.Megaparsec.Char (char,space,satisfy,string)
import Control.Applicative.Combinators (between)

data Term = Str String
          | Concat [Term]
          | Print Term deriving Show

term :: Parser Term
term = space *> (str <|> inparens (concat <|> print))

str    = Str    <$> (char '"' *> many (notChar '"') <* char '"')

concat = Concat <$> (char '+' *> many term)

print  = Print  <$> (string "print" *> term)

inparens = between (char '(') (char ')')

-- megaparsec 6.x
notChar = satisfy . (/=)

-- (print (+ "Hello " "World!")

--main :: IO ()
main = putStrLn $ show $ parse term "" "(print (+ \"Hello \" \"World!\"))"
