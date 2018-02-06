{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, LambdaCase #-}
module Step5 where

import Prelude (String,undefined,($),(.),(==),(/=))
import qualified Data.Functor as F



data Term = Str String
          | Concat [Term]
          | Print Term

term = space *> (str <|> inparens (concat <|> print))

inparens t = char '(' *> t <* char ')'

(<|>) = undefined
(<*>) = undefined

str    = Str    <$> (char '"' *> many (notChar '"') <* char '"')

concat = Concat <$> (char '+' *> many term)

print  = Print  <$> (string "print" *> term)




many = undefined
string = undefined


a *> b = (\_ x -> x) <$> a <*> b
a <* b = (\x _ -> x) <$> a <*> b

space = many $ char ' '







---------------------------------------------------------------------------------------------------

-- "a parser for things is a parser from strings to list of things and strings"
newtype Parser thing = Parser { parse :: String -> [(thing,String)] } deriving F.Functor
  
-- eat characters as long as a predicate matches
--satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \case
    x:xs | pred x -> [(x,xs)]
    _             -> []

char    = satisfy . (==)
notChar = satisfy . (/=)

(<$>) = (F.<$>)
