implementation module Text.Femtoparsec

import Base

import qualified Data.String as String
import qualified Data.Slice as Slice
from Data.Slice import :: Slice
import qualified Data.List as List

////////////////////////////////////////////////////////////////////////////////
/// # Types
////////////////////////////////////////////////////////////////////////////////

:: Parser a =: Parser (Slice -> Parsed a)
:: Parsed a
    = Done Slice a
    | Fail Slice Message
:: Message :== String

////////////////////////////////////////////////////////////////////////////////
/// # Running
////////////////////////////////////////////////////////////////////////////////

parseOnly :: (Parser a) String -> Either Message a
parseOnly (Parser p) s = eitherResult $ p $ 'Slice'.wrap s

maybeResult :: (Parsed a) -> Maybe a
maybeResult (Done _ a) = Just a
maybeResult (Fail _ _) = Nothing

eitherResult :: (Parsed a) -> Either Message a
eitherResult (Done _ a) = Right a
eitherResult (Fail _ e) = Left e

////////////////////////////////////////////////////////////////////////////////
/// # Instances
////////////////////////////////////////////////////////////////////////////////

instance Functor Parser where
    fmap f (Parser p)
        = Parser (\s -> case p s of
            Done s` a -> Done s` (f a)
            Fail s` e -> Fail s` e)

instance Applicative Parser where
    pure a = Parser (\s -> Done s a)

    (<*>) (Parser p) (Parser q) = Parser (\s -> case p s of
        Done s` f -> case q s` of
            Done s`` a -> Done s`` (f a)
            Fail s`` e -> Fail s`` e
        Fail s` e -> Fail s` e)

instance Alternative Parser where
    empty = Parser (\s -> Fail s "")

    (<|>) (Parser p) (Parser q) = Parser (\s -> case p s of
        Fail _ _ -> q s // Backtracking!
        done -> done)

////////////////////////////////////////////////////////////////////////////////
/// # Individual characters
////////////////////////////////////////////////////////////////////////////////

satisfy :: (Char -> Bool) -> Parser Char
satisfy t = Parser (\s -> case 'Slice'.uncons s of
    Just (c`,s`)
        | t c` -> Done s` c`
        | otherwise -> Fail s "satisfy: no match"
    Nothing -> Fail s "satisfy: empty string")

char :: Char -> Parser Char
char c = satisfy ((==) c)

notChar :: Char -> Parser Char
notChar c = satisfy ((<>) c)

anyChar :: Parser Char
anyChar = satisfy (const True)

////////////////////////////////////////////////////////////////////////////////
/// # Special characters
////////////////////////////////////////////////////////////////////////////////

isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c == ' ' || c == '\t'

isVerticalSpace :: Char -> Bool
isVerticalSpace c = '\n' <= c && c <= '\r' //= c == '\n' || c == '\v' || c == '\f' || c == '\r'

isEndOfLine :: Char -> Bool
isEndOfLine c = c == '\n' || c == '\r'

// digit :: Parser Char

// horizontalSpace :: Parser Char

// verticalSpace :: Parser Char

// space :: Parser Char

endOfLine :: Parser ()
endOfLine = void (char '\n') <|> void (string "\r\n")

restOfLine :: Parser ()
restOfLine = skipTill isEndOfLine *> endOfLine

endOfInput :: Parser ()
endOfInput = Parser (\s -> 'Slice'.isEmpty s ?
    Done s () $
    Fail s "endOfInput: there is more")

// endOfInput :: Parser ()
// endOfInput = Parser $ \s ->
//     | 'Slice'.isEmpty s = Done s ()
//     = Fail s "endOfInput: there is more"

// endOfInput = Parser $ \s -> case s of
//     "" -> Done s ()
//     _ -> Fail s "endOfInput: there is more"

////////////////////////////////////////////////////////////////////////////////
/// # String parsing
////////////////////////////////////////////////////////////////////////////////

string :: String -> Parser String
string p = Parser (\s -> if ('Slice'.isPrefixOf p s)
    (Done ('Slice'.drop ('String'.length p) s) p)
    (Fail s "string: no match"))

/// Not in Attoparsec
word :: Parser String
word = takeWhile isAlphanum

/// Not in Attoparsec
between :: Char Char -> Parser String
between o c = char o *> takeTill ((==) c) <* char c

/// ## Given number

take :: Int -> Parser String
take n = Parser (\s -> case 'Slice'.splitAt n s of
    (p,s`) -> Done s` ('Slice'.unwrap p)) // Never fails!

skip :: Int -> Parser ()
skip n = Parser (\s -> Done ('Slice'.drop n s) ()) // Never fails!

/// ## While predicate holds

takeWhile :: (Char -> Bool) -> Parser String
takeWhile t = Parser (\s -> case 'Slice'.span t s of
    (p,s`) -> Done s` ('Slice'.unwrap p)) // Never fails!

takeWhile1 :: (Char -> Bool) -> Parser String
takeWhile1 t = Parser (\s -> case 'Slice'.span t s of
    // ("",_) -> Fail s "takeWhile1: no match"
    // (p,s`) -> Done s` ('Slice'.unwrap p)
    (p,s`)
        | 'Slice'.isEmpty p -> Fail s "takeWhile1: no match"
        | otherwise -> Done s` ('Slice'.unwrap p))

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile t = Parser (\s -> Done ('Slice'.dropWhile t s) ()) // Never fails!

skipWhile1 :: (Char -> Bool) -> Parser ()
skipWhile1 t = void (takeWhile1 t)

/// ## Till predicate holds

takeTill :: (Char -> Bool) -> Parser String
takeTill t = takeWhile (not o t) // Never fails!

takeTill1 :: (Char -> Bool) -> Parser String
takeTill1 t = takeWhile1 (not o t)

skipTill :: (Char -> Bool) -> Parser ()
skipTill t = skipWhile (not o t) // Never fails!

skipTill1 :: (Char -> Bool) -> Parser ()
skipTill1 t = skipWhile1 (not o t)

/// ## Spaces

skipSpace :: Parser ()
skipSpace = skipWhile isSpace

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile isHorizontalSpace

skipVerticalSpace :: Parser ()
skipVerticalSpace = skipWhile isVerticalSpace

skipSpace1 :: Parser ()
skipSpace1 = skipWhile1 isSpace

skipHorizontalSpace1 :: Parser ()
skipHorizontalSpace1 = skipWhile1 isHorizontalSpace

skipVerticalSpace1 :: Parser ()
skipVerticalSpace1 = skipWhile1 isVerticalSpace

////////////////////////////////////////////////////////////////////////////////
/// # Numeric parsing
////////////////////////////////////////////////////////////////////////////////

decimal :: Parser Int
decimal = 'String'.foldl` step 0 <$> takeWhile1 isDigit
    where step last char = last * 10 + (toInt char - 48)

// hexadecimal :: Parser Int

// octal :: Parser Int

// rational :: Parser Real

// scientific :: (Parser a) -> Parser a
// exponentiated?

// signed :: (Parser a) -> Parser a

////////////////////////////////////////////////////////////////////////////////
/// # Combinators
////////////////////////////////////////////////////////////////////////////////

choice :: [f a] -> f a | Alternative f
// choice :: (t (f a)) -> f a | Traversable t, Applicative f
choice xs = 'List'.foldr (<|>) empty xs

// count :: Int (f a) -> f [a] | Alternative f

// option :: a (f a) -> f a | Alternative f

// many :: (f a) -> f [a] | Alternative f

many1 :: (f a) -> f [a] | Alternative f
many1 f = some f

// manyTill :: (Parser a) (Parser b) -> Parser [a]
manyTill :: (f a) (f b) -> f [a] | Alternative f
manyTill p end = scan
        where scan = (end *> pure []) <|> (\x xs -> [x:xs]) <$> p <*> scan

// sepBy :: (Parser a) (Parser s) -> Parser [a]
// sepBy :: (f a) (f s) -> f [a] | Alternative f

// sepBy1 :: (Parser a) (Parser s) -> Parser [a]
sepBy1 :: (f a) (f s) -> f [a] | Alternative f
sepBy1 p s = scan
    where scan = (\x xs -> [x:xs]) <$> p <*> ((s *> scan) <|> pure [])

/// Like `many`, but skip zero or more instances of an action.
skipMany :: (f a) -> f () | Alternative f
skipMany p = scan
    where scan = (p *> scan) <|> pure ()

/// Like `many1`, but skip one or more instances of an action.
skipMany1 :: (f a) -> f () | Alternative f
skipMany1 p = p *> skipMany p

