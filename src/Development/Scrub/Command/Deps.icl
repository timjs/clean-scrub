implementation module Development.Scrub.Command.Deps

import Data.Char
import Data.Bool
import Override.Data.Either
import Data.Func
import Data.String
import Data.Tuple
import qualified Data.List as List
from Data.List import instance Functor [], instance toString [], instance fromString []
import qualified Data.Set as Set
from Data.Set import :: Set

import Data.Functor
import Data.Foldable

import Control.Applicative
import qualified Control.Monad

import Text.Parsers.ParsersKernel, Text.Parsers.ParsersDerived

import System.Console.Output
import System.File
import System.FilePath

import Development.Scrub.Types

// Debugging //

derive gPrint MaybeError, Set

// # Running the command //

run :: [String] *World -> *World
run args world
    = seqSt showImportsOf args world

showImportsOf :: FilePath *World -> *World
showImportsOf path world
    # world = putAct ["Parsing imports of ", quote path] world
    # (result,world) = importsOf path world
    = case result of
        Right names -> seqSt putStrLn ('Set'.toList names) world
        Left error -> putErr [error] world

importsOf :: FilePath *World -> (Either String (Set Name), *World)
importsOf path world
    # (result,world) = readFile path world
    = (convert result >>= importsIn, world) //FIXME
    where
        convert :: (Either FileError a) -> Either String a
        convert (Left e) = Left (toString e)
        convert (Right a) = Right a

importsIn :: String -> Either String (Set Name)
importsIn string = 'Set'.fromList <$> parseOnly imports string

// Parser //

parseOnly :: (Parser Char r r) String -> Either String r
parseOnly parser input = toEither $ parse parser (fromString input) "parseOnly" "character"
    where
        toEither :: (Result r) -> Either String r
        toEither (Succ rs) = Right ('List'.head rs) //XXX uses `head`: unsafe for these parsers?
        toEither (Err a b c) = Left "!! Parse error" //(toString (a,b,c))

// Parser -- Helpers //

sepBy1 :: (f a) (f s) -> f [a] | Alternative f
sepBy1 p s = scan
    where scan = liftA2 'List'.cons p ((s *> scan) <|> pure [])

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

// Parser - Modules //

isName :: Char -> Bool
isName c = isAlphaNum c || c == '_' || c == '`' || c == '.'

name :: Parser Char t Name
name = toString <$> some (satisfy isName)

names :: Parser Char t [Name]
names = sepBy1 name (char ',' *> blank)

importLine :: Parser Char t [Name]
importLine = string "import" *> blank *> optional (string "qualified" *> blank) *> names <* rest

fromLine :: Parser Char t [Name]
fromLine = 'List'.singleton <$> (string "from" *> blank *> name <* rest)

otherLine :: Parser Char t [Name]
otherLine = rest *> pure []

imports :: Parser Char t [Name]
imports = concat <$> many (importLine <|> fromLine <|> otherLine)

