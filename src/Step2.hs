{-# LANGUAGE NoImplicitPrelude #-}
module Step2 where

import Prelude (String,undefined)




data Term = Str String
          | Concat [Term]
          | Print Term

term = str <|> inparens (concat <|> print)

inparens t = char '(' <*> t <*> char ')'

(<|>) = undefined
(<*>) = undefined







char    = undefined
notChar = undefined

many = undefined
string = undefined













---------------------------------------------------------------------------------------------------

-- "convert" Parser result to Term
(<$>) = undefined

str    = Str    <$> (char '"' <*> many (notChar '"') <*> char '"')
concat = Concat <$> (char '+' <*> many term)
print  = Print  <$> (string "print" <*> term)
