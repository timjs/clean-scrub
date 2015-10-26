definition module Text.Attoparsec

import Data.Either
import Data.String

import Data.Eq
import Data.Functor
from Control.Applicative import class Applicative(..), class Alternative(..), *>, <*

from System.File import :: FileError
from System.OSError import :: OSError, :: OSErrorCode, :: OSErrorMessage

from Text.Parsers.Parsers import :: Parser,
    symbol, token, <!*>, <!+>, <!?>, number, satisfy,
    instance Functor Parser, instance Applicative Parser, instance Alternative Parser

// Parser -- Run //

parseOnly :: (Parser Char r r) String -> Either String r

// Parser -- Helpers //

sepBy1 :: (f a) (f s) -> f [a] | Alternative f

// char :: Char -> Parser Char t Char
char c :== symbol c
// string :: String -> Parser Char t [Char]
string s :== token (fromString s)
// many :: (Parser s t r) -> Parser s t [r]
many p :== <!*> p
// some :: (Parser s t r) -> Parser s t [r]
some p :== <!+> p
// optional :: (Parser s t r) -> Parser s t ()
optional p :== <!?> p (\_ -> ()) ()

// decimal :: Parser Char a Int
decimal :== number

// Parser -- Spaces //

isHorizontalSpace :: Char -> Bool
horizontalSpace :: Parser Char t Char
blank :: Parser Char t ()
isEndOfLine :: Char -> Bool
endOfLine :: Parser Char t ()

rest :: Parser Char t ()

