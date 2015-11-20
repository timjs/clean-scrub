definition module Text.Femtoparsec

from Data.Either import :: Either
from Data.Maybe import :: Maybe

from Data.Functor import class Functor
from Control.Applicative import class Applicative, class Alternative

from Data.Slice import :: Slice

////////////////////////////////////////////////////////////////////////////////
/// # Types
////////////////////////////////////////////////////////////////////////////////

:: Parser a =: Parser (Slice -> Parsed a)
:: Parsed a
:: Message :== String

////////////////////////////////////////////////////////////////////////////////
/// # Running
////////////////////////////////////////////////////////////////////////////////

parseOnly :: (Parser a) String -> Either Message a
// maybeResult :: (ParseResult a) -> Maybe a
// eitherResult :: (ParseResult a) -> Either Message a

////////////////////////////////////////////////////////////////////////////////
/// # Instances
////////////////////////////////////////////////////////////////////////////////

instance Functor Parser
instance Applicative Parser
instance Alternative Parser

////////////////////////////////////////////////////////////////////////////////
/// # Individual characters
////////////////////////////////////////////////////////////////////////////////

satisfy :: (Char -> Bool) -> Parser Char
char :: Char -> Parser Char
notChar :: Char -> Parser Char
anyChar :: Parser Char

////////////////////////////////////////////////////////////////////////////////
/// # Special characters
////////////////////////////////////////////////////////////////////////////////

isHorizontalSpace :: Char -> Bool
isVerticalSpace :: Char -> Bool
isEndOfLine :: Char -> Bool
// digit :: Parser Char
// horizontalSpace :: Parser Char
// verticalSpace :: Parser Char
// space :: Parser Char
endOfLine :: Parser ()
restOfLine :: Parser ()
endOfInput :: Parser ()

////////////////////////////////////////////////////////////////////////////////
/// # String parsing
////////////////////////////////////////////////////////////////////////////////

string :: String -> Parser String
/// Not in Attoparsec
word :: Parser String
/// Not in Attoparsec
between :: Char Char -> Parser String

/// ## Given number

take :: Int -> Parser String
skip :: Int -> Parser ()

/// ## While predicate holds

takeWhile :: (Char -> Bool) -> Parser String
takeWhile1 :: (Char -> Bool) -> Parser String
skipWhile :: (Char -> Bool) -> Parser ()
skipWhile1 :: (Char -> Bool) -> Parser ()

/// ## Till predicate holds

takeTill :: (Char -> Bool) -> Parser String
takeTill1 :: (Char -> Bool) -> Parser String
skipTill :: (Char -> Bool) -> Parser ()
skipTill1 :: (Char -> Bool) -> Parser ()

/// ## Spaces

skipSpace :: Parser ()
skipHorizontalSpace :: Parser ()
skipVerticalSpace :: Parser ()

skipSpace1 :: Parser ()
skipHorizontalSpace1 :: Parser ()
skipVerticalSpace1 :: Parser ()

////////////////////////////////////////////////////////////////////////////////
/// # Numeric parsing
////////////////////////////////////////////////////////////////////////////////

decimal :: Parser Int
// hexadecimal :: Parser Int
// octal :: Parser Int

// natural :: Parser Int
// rational :: Parser Real
// scientific :: (Parser a) -> Parser a
// signed :: (Parser a) -> Parser a

////////////////////////////////////////////////////////////////////////////////
/// # Combinators
////////////////////////////////////////////////////////////////////////////////

choice :: [f a] -> f a | Alternative  f
// count :: Int (f a) -> f [a] | Alternative f
// option :: a (f a) -> f a | Alternative f
// many :: (f a) -> f [a] | Alternative f
many1 :: (f a) -> f [a] | Alternative f
manyTill :: (f a) (f b) -> f [a] | Alternative f
// sepBy :: (f a) (f s) -> f [a] | Alternative f
sepBy1 :: (f a) (f s) -> f [a] | Alternative f
/// Like `many`, but skip zero or more instances of an action.
skipMany :: (f a) -> f () | Alternative f
/// Like `many1`, but skip one or more instances of an action.
skipMany1 :: (f a) -> f () | Alternative f

