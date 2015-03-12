-- testing ground for building parsers.

-- following a bit the London Haskell Meetup talk on parsing.
-- parses JSON

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative
import Control.Monad

data JSONValue = B Bool
               | S String
               | A [JSONValue] 
               | O [(JSONValue, JSONValue)] 
               | I Integer
               deriving (Show)

-- Parsing Booleans

matchTrue :: Parser String
matchTrue = string "true"

matchFalse :: Parser String
matchFalse = string "false"

alwaysTrue :: Parser Bool
alwaysTrue = pure True

alwaysFalse :: Parser Bool
alwaysFalse = pure False

-- inlining the functions above for readability
parseTrue :: Parser Bool
parseTrue = string "true" *> pure True

parseFalse :: Parser Bool
parseFalse = string "false" *> pure False

bool :: Parser JSONValue
bool = B <$> parseTrue <|> B <$> parseFalse

-- parse Strings
stringLiteral :: Parser JSONValue 
stringLiteral = S <$> (char '"' *> (many (noneOf [ '"'])) <* char '"')

-- parse Array
-- figured this one out myself!

commaStripped :: Parser ()
commaStripped = do
    skipMany (char ' ')
    char ','
    skipMany (char ' ')

parseArrayUnit :: Parser JSONValue
parseArrayUnit = commaStripped *> value <* commaStripped 

array :: Parser JSONValue
array = A <$> (char '[' *> (many parseArrayUnit) <* char ']')  

-- using sepBy
array' :: Parser JSONValue
array' = A <$> (char '[' *> value `sepBy` commaStripped <* char ']')

-- Object (very similar to the talk)

colonStripped :: Parser ()
colonStripped = do 
    skipMany (char ' ')
    char ':'
    skipMany (char ' ')

parseObjectEntry :: Parser (JSONValue, JSONValue)
parseObjectEntry = do
    objKey <- stringLiteral
    colonStripped
    objValue <- value
    return (objKey, objValue)

object :: Parser JSONValue
object = O <$> (char '{' *> (parseObjectEntry `sepBy` commaStripped) <* char '}')

-- integer
-- tricky... what happens if the string starts with digit, but ends with alphanum?
-- then I'd want it fail.
integer :: Parser JSONValue
integer = I <$> read <$> integer'
    where integer' = do
                     lead <- (oneOf "-0123456789")
                     follow <- many digit
                     return (lead: follow)

-- sexy! From FP complete tutorial, using more applicatives.
integer' :: Parser JSONValue
integer' = I <$> read <$> (plus <|> minus <|> number)
    where plus   = char '+' *> number
          minus  = (:) <$> char '-' <*> number
          number = many1 (oneOf "0123456789") 

-- final parse
value :: Parser JSONValue
value = bool <|> stringLiteral <|> array <|> object <|> integer
