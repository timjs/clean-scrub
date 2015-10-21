implementation module Development.Scrub.Module

import Data.Char
import Data.Bool
import Data.Func
import Data.Maybe
import Data.Result
import Data.String
import Data.Tuple
import Data.Eq
import Data.Ord
import Data.Foldable
import Data.Traversable

import qualified Data.List as List
from Data.List import instance Functor [], instance toString [], instance fromString []
import qualified Data.Set as Set
import Data.Set.Operators
import qualified Data.Map as Map
from Data.Map import :: Map, instance Semigroup Map, instance Monoid Map

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
    # world = putAct ["Calculating imports of", quote path] world
    # (result,world) = calcImports path world
    | isError result = putErr [toString $ fromError result] world
    # names = fromOk result
    = seqSt putStrLn ('Set'.toList names) world

calcImports :: FilePath *World -> *Return (Set Name)
calcImports path world
    # world = logInf ["Reading contents of", quote path] world
    # (result,world) = readFile path world
    | isError result = (rethrow` result, world)
    # string = fromOk result
    = (parseImports string, world)

parseImports :: String -> Result Error (Set Name)
parseImports string = mapBoth ParseError 'Set'.fromList $ parseOnly imports string

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

// showDependencies path = readMainManifest >>= createDatabase >>= calcDependencies path >>= showDatabase
showDependencies :: FilePath *World -> *World
showDependencies path world
    # world = logAct ["Calculating dependecies of", quote path] world
    # (result,world) = readMainManifest world
    | isError result = world
    # manifest = fromOk result
    # (result,world) = createDatabase manifest world
    | isError result = world
    # database = fromOk result
    # (result,world) = calcDependencies path database world
    | isError result = putErr [toString $ fromError result] world
    # names = fromOk result
    = seqSt putStrLn ('Set'.toList names) world

//TODO optimise by collecting in DependencyTree ?
calcDependencies :: FilePath Database *World -> *Return (Set Name)
calcDependencies path database world
    # (result,world) = calcImports path world
    | isError result = (result, world)
    # todo = fromOk result
    = go todo 'Set'.empty world
    where
        // go :: (Set a) -> (Set a) *World -> *Return (Set Name)
        go todo done world
            | 'Set'.null todo = (Ok done, world)
            # (current,rest) = 'Set'.deleteFindMin todo
            # result = lookupModule current database
            | isError result = (rethrow result, world)
            # path = fromOk result
            # (result,world) = calcImports path world
            | isError result = (result, world)
            # imports = fromOk result
            # todo = rest \/ imports \\\ done
            # done = 'Set'.insert current done
            = go todo done world

//
// # Module database
//

definitionExtension :== "dcl" //XXX move elsewhere
moduleSeparator :== '.'

//XXX move elsewhere
replace :: Char Char -> String -> String
replace x y = toString o 'List'.map (\e -> e == x ? y $ e) o fromString

createDatabase :: Manifest *World -> *Return Database
createDatabase manifest world
    # world = logInf ["Creating main module database"] world
    # (results,world) = mapSt createPackage manifest.dependencies world
    # result = sequence results
    | isError result = (rethrow result, world)
    # packages = fromOk result
    # (result,world) = localModules manifest world
    | isError result = (result, world)
    // putErr ["Error creating database:", error] world
    # database = fromOk result
    = (Ok $ 'List'.foldr extendDatabase database packages, world)

// createDatabase manifest world
//     tell [Info "Creating main module database"]
//     packages <- sequence $ traverse createPackage manifest.dependencies
//     database <- localModules manifest
//     return $ 'List'.foldr extendDatabase database packages

extendDatabase :: Package Database -> Database
extendDatabase package database
    // traceAct ["Extending module database with", quote package.manifest.package.BasicInfo.name] $
    = 'List'.foldr (uncurry 'Map'.insert) database $ 'List'.zip2 moduleNames definitionPaths
    where
        moduleNames = maybe [] (\info -> info.modules) package.manifest.library
        definitionPaths = 'List'.map transform moduleNames
        transform name = package.Package.path </> maybe "" id package.manifest.package.sources </> replace moduleSeparator pathSeparator name <.> definitionExtension
        //XXX someday: transform name = scrubPackageRoot </> package.name </> package.version </> package.sources </> replace moduleSeparator pathSeparator name <.> definitionExtension

localModules :: Manifest *World -> *Return Database
localModules manifest world
    # world = logInf ["Searching for local modules"] world
    # world = logRes ["Source directory"] sourceDir world
    # (result,world) = findFiles definitionPredicate sourceDir world
    | isError result = (rethrow` result, world)
    # definitionPaths = fromOk result
    # moduleNames = 'List'.map transform definitionPaths
    # world = logRes ["Found local modules"] moduleNames world
    = (Ok $ 'Map'.fromList $ 'List'.zip2 moduleNames definitionPaths, world)
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

lookupModule :: Name Database -> Result Error FilePath
lookupModule module database
    = case 'Map'.lookup module database of
        Nothing -> Error $ LookupError $ "Could not find module " +++ quote module +++ " in packages"
        Just path -> Ok path

