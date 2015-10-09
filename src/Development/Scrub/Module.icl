implementation module Development.Scrub.Module

import Data.Char
import Data.Bool
import Data.Either
import Data.Func
import Data.String
import Data.Tuple
import qualified Data.List as List
from Data.List import instance Functor [], instance toString [], instance fromString []
import qualified Data.Set as Set
from Data.Set import :: Set

import Data.Foldable

import System.Console.Output
import System.File
import System.FilePath

import Development.Scrub.Types
import Development.Scrub.Parsers

// # Running the command //

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

