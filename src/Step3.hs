{-# LANGUAGE NoImplicitPrelude #-}
module Step3 where

import Prelude (String,undefined)




data Term = Str String
          | Concat [Term]
          | Print Term

term = str <|> inparens (concat <|> print)



(<|>) = undefined
(<*>) = undefined







char    = undefined
notChar = undefined

many = undefined
string = undefined

(<$>) = undefined











---------------------------------------------------------------------------------------------------

-- "ignoring left and taking right"
a *> b = (\_ x -> x) <$> a <*> b

-- "taking left and ignoring right
a <* b = (\x _ -> x) <$> a <*> b

str    = Str    <$> (char '"' *> many (notChar '"') <* char '"')
concat = Concat <$> (char '+' *> many term)
print  = Print  <$> (string "print" *> term)

inparens t = char '(' *> t <* char ')'
