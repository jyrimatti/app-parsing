{-# LANGUAGE NoImplicitPrelude #-}
module Step1 where

import Prelude (String,undefined)


-- (print (+ "Hello " "World!")

data Term = Str String
          | Concat [Term]
          | Print Term

term = str <|> inparens (concat <|> print)

inparens t = char '(' <*> t <*> char ')'

(<|>) = undefined
(<*>) = undefined

str = char '"' <*> many (notChar '"') <*> char '"'

concat = char '+' <*> many term

print = string "print" <*> term

char    = undefined
notChar = undefined

many = undefined
string = undefined
