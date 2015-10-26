implementation module Text.Attoparsec

import Data.Bool
import Data.Char
import Data.Either
import Data.String
import qualified Data.List as List
from Data.List import instance Functor [], instance toString [], instance fromString []

import Data.Eq
import Data.Func
import Data.Functor

from Control.Applicative import class Applicative(..), class Alternative(..), *>, <*, liftA2

from System.File import :: FileError
from System.OSError import :: OSError, :: OSErrorCode, :: OSErrorMessage

import Text.Parsers.Parsers

// Parser -- Run //

parseOnly :: (Parser Char r r) String -> Either String r
parseOnly parser input = toEither $ parse parser (fromString input) "parseOnly" "character"
    where
        toEither :: (Result r) -> Either String r
        toEither (Succ rs) = Right ('List'.head rs) //XXX uses `head`, but because these are non-deterministic parsers it is ok?
        toEither (Err a b c) = Left "Parser combinators failed" //(toString (a,b,c))

// Parser -- Helpers //

sepBy1 :: (f a) (f s) -> f [a] | Alternative f
sepBy1 p s = scan
    where scan = liftA2 'List'.cons p ((s *> scan) <|> pure [])

// Parser -- Spaces //

isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c == ' ' || c == '\t'

horizontalSpace :: Parser Char t Char
horizontalSpace = satisfy isHorizontalSpace

blank :: Parser Char t ()
blank = some horizontalSpace *> pure () //skipOver horizontalSpace

isEndOfLine :: Char -> Bool
isEndOfLine c = c == '\n' || c == '\r'

endOfLine :: Parser Char t ()
endOfLine = (char '\n' *> pure ()) <|> (string "\r\n" *> pure ())

rest :: Parser Char t ()
rest = skipTo endOfLine *> endOfLine

