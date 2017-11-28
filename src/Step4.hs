{-# LANGUAGE NoImplicitPrelude #-}
module Step4 where

import Prelude (String,undefined,($))




data Term = Str String
          | Concat [Term]
          | Print Term



inparens t = char '(' *> t <* char ')'

(<|>) = undefined
(<*>) = undefined

str    = Str    <$> (char '"' *> many (notChar '"') <* char '"')

concat = Concat <$> (char '+' *> many term)

print  = Print  <$> (string "print" *> term)

char    = undefined
notChar = undefined

many = undefined
string = undefined

(<$>) = undefined
a *> b = (\_ x -> x) <$> a <*> b
a <* b = (\x _ -> x) <$> a <*> b









---------------------------------------------------------------------------------------------------

term = space *> (str <|> inparens (concat <|> print))

space = many $ char ' '
