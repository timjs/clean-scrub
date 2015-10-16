implementation module Development.Scrub.Module

import Data.Char
import Data.Bool
import Data.Either
import Data.Func
import Data.Maybe
import Data.String
import Data.Tuple
import Data.Eq
import Data.Ord

import qualified Data.List as List
from Data.List import instance Functor [], instance toString [], instance fromString []
import qualified Data.Set as Set
import Data.Set.Operators
import qualified Data.Map as Map
from Data.Map import :: Map, instance Semigroup Map, instance Monoid Map

import Data.Foldable

import System.Console.Output
import System.File
import System.FilePath
import System.FilePath.Find
import System.Process

import Development.Scrub.Manifest
import Development.Scrub.Types
import Development.Scrub.Parsers

//
// # Finding imports
//

showImports :: FilePath *World -> *World
showImports path world
    # (result,world) = calcImports path world
    = case result of
        Right names -> seqSt putStrLn ('Set'.toList names) world
        Left error -> putErr ["Error calculating imports:", error] world

calcImports :: FilePath *World -> (Either String (Set Name), *World)
calcImports path world
    # (result,world) = traceAct ["Reading contents of", quote path] $
        readFile path world
    = (convert result >>= parseImports, world) //FIXME
    where
        convert :: (Either FileError a) -> Either String a
        convert (Left e) = Left (toString e)
        convert (Right a) = Right a

parseImports :: String -> Either String (Set Name)
parseImports string = 'Set'.fromList <$> parseOnly imports string

//
// ## Imports parser
//

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

//
// # Calculating dependencies
//

showDependencies :: FilePath *World -> *World
showDependencies path world
    # (manifest,world) = readMainManifest world
    # (database,world) = createDatabase manifest world
    # (result,world) = calcDependencies path database world
    = case result of
        Right names -> seqSt putStrLn ('Set'.toList names) world //FIXME DRY
        Left error -> putErr ["Error calculating dependencies:", error] world

//TODO optimise by collecting in DependencyTree
calcDependencies :: FilePath Database *World -> (Either String (Set Name), *World)
calcDependencies path database world
    # (result,world) = traceAct ["Calculating dependencies of", quote path] $
        calcImports path world
    = case result of
        Right todo -> go todo 'Set'.empty world
        error -> (error, world)
    where
        // go :: (Set a) (Set a) *World -> (Either String (Set Name), *World)
        go todo done world
            | 'Set'.null todo = (pure done, world)
            # (current,rest) = 'Set'.deleteFindMin todo
            # pathE = lookupDatabase current database
            = case pathE of
                Right path
                    # (importsE,world) = calcImports path world
                    # todoE = importsE >>= \imports ->
                        pure $ rest \/ imports \\\ done
                    # done = 'Set'.insert current done
                    = case todoE of
                        Right todo = go todo done world
                        error = (error, world)
                Left e = (Left e, world)

throw e :== Left e

//
// # Module database
//

definitionExtension :== "dcl" //XXX move elsewhere
moduleSeparator :== '.'

//XXX move elsewhere
replace :: Char Char -> String -> String
replace x y = toString o 'List'.map (\e -> e == x ? y $ e) o fromString

createDatabase :: Manifest *World -> (Database, *World)
createDatabase manifest world
    # (packages,world) = mapSt createPackage manifest.dependencies world
    # (result,world) = localModules manifest world
    = case result of
        Left error
            # world = putErr ["Error creating database:", error] world
            = exit 1 world
        Right database
            = traceAct ["Creating main module database"] $
                ('List'.foldr extendDatabase database packages, world)

extendDatabase :: Package Database -> Database
extendDatabase package database
    = traceAct ["Extending module database with", quote package.manifest.package.BasicInfo.name] $
        'List'.foldr (uncurry 'Map'.insert) database $ 'List'.zip2 moduleNames definitionPaths
    where
        moduleNames = maybe [] (\info -> info.modules) package.manifest.library
        definitionPaths = 'List'.map transform moduleNames
        transform name = package.Package.path </> maybe "" id package.manifest.package.sources </> replace moduleSeparator pathSeparator name <.> definitionExtension
        //XXX someday: transform name = scrubPackageRoot </> package.name </> package.version </> package.sources </> replace moduleSeparator pathSeparator name <.> definitionExtension

localModules :: Manifest *World -> (Either String Database, *World)
localModules manifest world
    # (result,world) = findFiles definitionPredicate sourceDir world
    = case result of
        Left error = (throw $ "Some OSError...", world)
        Right definitionPaths
            # moduleNames = traceRes ["Found local modules"] $
                'List'.map transform definitionPaths
            = traceAct ["Searching for local modules"] $
                (pure $ 'Map'.fromList $ //traceRes ["Local modules"] $
                    'List'.zip2 moduleNames definitionPaths
                , world)
    where
        sourceDir = maybe "./src" id manifest.package.sources
        transform = replace pathSeparator moduleSeparator o makeRelative sourceDir o dropExtension
        definitionPredicate info = takeExtension info.FileInformation.path == definitionExtension

// mkDatabase :: [Package] -> Database
// mkDatabase packages
//     = foldMap extractDatabase packages //XXX add duplication check of modules
// extractDatabase :: Package -> Database
// extractDatabase package
//     = 'Map'.fromList $ 'List'.zip2 names paths
//     where ...

//
// ## Resolving module definitionPaths
// 

lookupDatabase :: Name Database -> Either String FilePath
lookupDatabase module database
    = case 'Map'.lookup module database of
        Nothing -> throw $ "Could not find module " +++ quote module +++ " in packages"
        Just path -> pure path

