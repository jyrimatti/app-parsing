{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, DeriveFunctor #-}
module Main where

import Prelude (String,($),(.),(/=),Show,show,putStrLn)

import Data.Functor ((<$>))
import Control.Applicative (many,(<|>),(*>),(<*))
import Control.Applicative.Combinators (between)

import Text.Megaparsec (parse)
import Text.Megaparsec.String (Parser)
import Text.Megaparsec.Char (char,space,satisfy,string)


data Term = Str String
          | Concat [Term]
          | Print Term
    deriving Show

term :: Parser Term
term = space *> (str <|> inparens (concat <|> print))

str    = Str    <$> (char '"' *> many (notChar '"') <* char '"')

concat = Concat <$> (char '+' *> many term)

print  = Print  <$> (string "print" *> term)

inparens = between (char '(') (char ')')

-- included in megaparsec 6.x
notChar = satisfy . (/=)

main = putStrLn $ show $ parse term "" "(print (+ \"Hello \" \"World!\"))"
