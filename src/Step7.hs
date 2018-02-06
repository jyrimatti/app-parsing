{-# LANGUAGE NoImplicitPrelude, DeriveFunctor, GeneralizedNewtypeDeriving, LambdaCase #-}
module Step7 where

import Prelude (String,          ($),(.),(==),(/=))
import Data.Maybe (Maybe)
import Data.Functor (Functor)
import Text.Show (Show)
import Control.Applicative (Applicative,Alternative,pure,empty,(<$>),(<|>),(*>),(<*),many)
import qualified Data.Traversable as T
import qualified Control.Monad.Trans.State.Strict as ST

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






string = T.traverse char
space = many $ char ' '

-- http://vaibhavsagar.com/blog/2018/02/04/revisiting-monadic-parsing-haskell/
newtype Parser thing = Parser { unParser :: ST.StateT String Maybe thing } deriving (Functor, Applicative, Alternative)

parse = ST.runStateT . unParser

satisfy pred = Parser . ST.StateT $ \case
    x:xs | pred x -> pure (x,xs)
    _             -> empty

test = parse term "(print (+ \"Hello \" \"World!\"))"