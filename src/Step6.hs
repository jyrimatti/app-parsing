{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, LambdaCase #-}
module Step6 where

import Prelude (String,          ($),(.),(==),(/=),Show,Maybe(Just,Nothing))
import qualified Data.Functor as F
import qualified Control.Applicative as A
import qualified Data.Traversable as T

data Term = Str String
          | Concat [Term]
          | Print Term deriving Show

term = space *> (str <|> inparens (concat <|> print))

inparens t = char '(' *> t <* char ')'




str    = Str    <$> (char '"' *> many (notChar '"') <* char '"')

concat = Concat <$> (char '+' *> many term)

print  = Print  <$> (string "print" *> term)

char    = satisfy . (==)
notChar = satisfy . (/=)




(<$>) = (F.<$>)



space = many $ char ' '

newtype Parser thing = Parser { parse :: String -> [(thing,String)] } deriving F.Functor

satisfy pred = Parser $ \case
    x:xs | pred x -> [(x,xs)]
    _             -> []

---------------------------------------------------------------------------------------------------

(<|>) = (A.<|>)
a <*> b = a A.<*> b

(*>) = (A.*>)
(<*) = (A.<*)

many = A.many

instance A.Applicative Parser where
  pure x = Parser $ \input -> [(x, input)]
  Parser af <*> Parser aa = Parser $ \input ->
     [(f a, input2) | (f, input1) <- af input, (a, input2) <- aa input1]
     

instance A.Alternative Parser where
  empty = Parser $ \_ -> []
  (Parser p) <|> (Parser q) = Parser $ \input ->
    case p input of
      [] -> q input
      r  -> r

string = T.traverse char

parseProgram s = case parse term s of
  [(t,"")] -> Just t
  _        -> Nothing
  
-- > parseProgram "(print (+ \"Hello \" \"World!\"))"
-- Just (Print (Concat [Str "Hello ",Str "World!"]))