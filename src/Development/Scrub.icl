implementation module Development.Scrub

//TODO make Prelude
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
// import Data.Map.Operators
from Data.Map import :: Map //, instance Semigroup Map, instance Monoid Map

import System.Console.Output
import System.File
import System.FilePath
import System.FilePath.Find
import System.OSError
import System.Process

import Text.Attoparsec
import Text.JSON

////////////////////////////////////////////////////////////////////////////////
/// # Version
////////////////////////////////////////////////////////////////////////////////

parseVersion :: String -> Either String Version
parseVersion input = parseOnly parser input
    where
        parser = Version <$> decimal <* char '.' <*> decimal <* char '.' <*> decimal

instance toString Version where
    toString (Version x y z) = toString x +++ "." +++ toString y +++ "." +++ toString z

// JSONDecode{|Version|} :: !Bool ![JSONNode] -> (!Maybe Version,![JSONNode])
JSONDecode{|Version|} _ nodes=:[JSONString candidate : rest]
    = case parseVersion candidate of
        Left _ = (Nothing, nodes)
        Right version = (Just version, rest)
JSONDecode{|Version|} _ nodes
    = (Nothing, nodes)

// JSONEncode{|Version|} :: !Bool !Version -> [JSONNode]
JSONEncode{|Version|} _ version
    = [JSONString $ toString version]

////////////////////////////////////////////////////////////////////////////////
/// # Errors
////////////////////////////////////////////////////////////////////////////////

derive JSONDecode Error, Either, FileError
derive JSONEncode Error, Either, FileError

//TODO improve error messages
instance toString Error where
    toString (SystemError (error, message))
        = "A system error occured: (E" +++ toString error +++ ") " +++ message
    toString (FileError path CannotOpen)
        = "Can not open " +++ quote path +++ ", no such file or directory"
    toString (FileError path CannotClose)
        = "Can not close " +++ quote path
    toString (FileError path IOError)
        = "An I/O error occured during modifcation of " +++ quote path
    toString (ParseError message)
        = "Parse error: " +++ message
    toString (LookupError message)
        = "Lookup error: " +++ message
    toString (NinjaError message)
        = "Ninja error: " +++ message
    toString (PackageError name modules)
        = "Package " +++ name +++ " exports modules without implementation: " +++ show modules

////////////////////////////////////////////////////////////////////////////////
/// # Packages
////////////////////////////////////////////////////////////////////////////////

derive JSONEncode Package
derive JSONDecode Package

/// ## Initialisers

createPackage :: FilePath *World -> *Return Package
createPackage path world
    # world = logInf ["Creating package info for", quote path] world
    # (result,world) = readManifest path world
    | isError result = (rethrow id result, world)
    # manifest = fromOk result
    # name = manifest.info.BasicInfo.name
    # modules = maybe [] (\lib -> lib.LibraryInfo.modules) manifest.library
    # world = logRes ["Exported modules from", quote name] modules world
    # sourceDirs = maybe [defaultSourceDir] id manifest.info.BasicInfo.sourceDirs
    # sourceDirs = 'List'.map (combine path) sourceDirs
    # world = logRes ["Source directories for", quote name] sourceDirs world
    # (results,world) = mapSt findLocalModules sourceDirs world
    # result = sequence results
    | isError result = (rethrow id result, world)
    # locals = 'Map'.unions $ fromOk result
    # exports = 'Map'.fromList $ 'List'.zip2 modules ('List'.repeat "")
    # missing = 'Map'.difference exports locals
    | not $ 'Map'.null missing = (throw $ PackageError manifest.info.BasicInfo.name ('Map'.keys missing), world)
    = (Ok
        { Package
        | name = name
        , version = manifest.info.BasicInfo.version
        , authors = manifest.info.BasicInfo.authors
        , path = path
        , sourceDirs = sourceDirs
        , dependencies = maybe [] id manifest.Manifest.dependencies //TODO add clean-base as implicit dependencie for every package?
        , executables = maybe [defaultExecutable manifest.info.BasicInfo.name] id manifest.Manifest.executables
        , locals = locals
        , exports = 'Map'.intersection locals exports
        // , dictionary = locals \/ exports /\ exports // Union on Maps is left biased! Exported modules without a .dcl now have an empty path.
        }, world)

createPackageFromDependency :: DependencyInfo *World -> *Return Package
createPackageFromDependency info world
    # path = info.DependencyInfo.path //TODO change to dir in registry
    = createPackage path world

showMainPackage :: *World -> *World
showMainPackage world
    # (result,world) = createPackage "." world
    | isError result = putErr [toString $ fromError result] world
    # package = fromOk result
    = showPackage package world

showMainModuleDictionary :: *World -> *World
showMainModuleDictionary world
    # (result,world) = createPackage "." world
    | isError result = putErr [toString $ fromError result] world
    # package = fromOk result
    # (result,world) = createModuleDictionary package world
    | isError result = putErr [toString $ fromError result] world
    # dictionary = fromOk result
    = putStrLn (pretty $ 'Map'.toList dictionary) world 

/// ## Methods

showPackage :: Package *World -> *World
showPackage package world
    # world = putAct ["Package information for", package.Package.name] world
    = putStrLn (pretty $ package) world

/// ### Module Imports

showModuleImports :: FilePath *World -> *World
showModuleImports path world
    # world = putAct ["Calculating imports of", quote path] world
    # (result,world) = calculateModuleImports path world
    | isError result = putErr [toString $ fromError result] world
    # imports = fromOk result
    = seqSt putStrLn ('Set'.toList imports) world

calculateModuleImports :: FilePath *World -> *Return (Set Name)
calculateModuleImports path world
    # world = logInf ["Reading contents of", quote path] world
    # (result,world) = readFile path world
    | isError result = (rethrow (FileError path) result, world)
    # string = fromOk result
    = (parseModuleImports string, world)

parseModuleImports :: String -> Result Error (Set Name)
parseModuleImports string = mapBoth ParseError 'Set'.fromList $ parseOnly imports string

/// ### Module Database

createModuleDictionary :: Package *World -> *Return Dictionary
createModuleDictionary package world
    # world = logInf ["Creating module dictionary"] world
    # (results,world) = mapSt addPackageDependency package.Package.dependencies world
    # result = sequence results
    | isError result = (rethrow id result, world)
    # others = fromOk result
    # dictionary = 'List'.foldr 'Map'.union package.locals others
    = (Ok dictionary, world)

addPackageDependency :: DependencyInfo *World -> *Return Dictionary
addPackageDependency dependency world
    # world = logInf ["Adding exported modules from", dependency.DependencyInfo.name, "version", dependency.DependencyInfo.version] world
    # (result,world) = findPackage dependency.DependencyInfo.path world //TODO someday resolve by name and version
    | isError result = (rethrow id result, world)
    # package = fromOk result
    # exports = package.exports
    # world = logRes ["Added modules from", quote dependency.DependencyInfo.name] exports world
    = (Ok exports, world)

/// ### Module Dependencies

// showModuleDependencies :: FilePath Package *World -> *World
// showModuleDependencies path package world
//     # world = putAct ["Calculating dependencies of", quote path] world
//     = seqSt putStrLn ('Set'.toList $ calculateModuleDependencies path) world
    
// calculateModuleDependencies :: FilePath Package *World -> *Return (Set Name)
// calculateModuleDependencies path package world
//     # (result,world) = calculateModuleImports path world
//     | isError result = (result, world)
//     # todo = fromOk result
//     = go todo 'Set'.empty world
//     where
//         // go :: (Set a) (Set a) *World -> *Return (Set Name)
//         go todo done world
//             | 'Set'.null todo = (Ok done, world)
//             # (current,rest) = 'Set'.deleteFindMin todo
//             # result = lookupModule current dictionary
//             | isError result = (rethrow result, world)
//             # path = fromOk result
//             # (result,world) = calculateModuleImports path world
//             | isError result = (result, world)
//             # imports = fromOk result
//             # todo = rest \/ imports \\\ done
//             # done = 'Set'.insert current done
//             = go todo done world

/// # Package Registry

/// ## Helpers

//TODO someday :: Name Version *World -> *Return Package
findPackage :: FilePath *World -> *Return Package
findPackage path world
    # world = logInf ["Looking up package in", quote path] world
    // # world = logInf ["Looking up package", name, ", version:", version] world
    # (result,world) = createPackage path world
    | isError result = (rethrow id result, world)
    # package = fromOk result
    = (Ok package, world)

findLocalModules :: FilePath *World -> *Return Dictionary
findLocalModules sourceDir world
    # world = logInf ["Looking up local modules in", quote sourceDir] world
    # (result,world) = findFiles definitionPredicate sourceDir world
    | isError result = (rethrow SystemError result, world)
    # definitionPaths = fromOk result
    # moduleNames = 'List'.map translate definitionPaths
    # world = logRes ["Found local modules"] moduleNames world
    = (Ok $ 'Map'.fromList ('List'.zip2 moduleNames definitionPaths), world)
    where
        translate = replace pathSeparator moduleSeparator o makeRelative sourceDir o dropExtension
        definitionPredicate info = takeExtension info.FileInformation.path == definitionExtension

/// ### Import parser

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

////////////////////////////////////////////////////////////////////////////////
/// # Manifest
////////////////////////////////////////////////////////////////////////////////

derive JSONDecode Manifest, BasicInfo, DependencyInfo, LibraryInfo, ExecutableInfo
derive JSONEncode Manifest, BasicInfo, DependencyInfo, LibraryInfo, ExecutableInfo

/// ## Initialisers

readManifest :: FilePath *World -> *Return Manifest
// readManifest path = ... fromJSON $ fromString <$> readFile (path </> manifestFilename)
readManifest path world
    # world = logInf ["Reading manifest file from", quote path] world
    # (result,world) = readFile (path </> manifestFilename) world
    // putErr ["Error reading manifest file from", quote path, ":", toString error] world
    | isError result = (rethrow (FileError path) result, world)
    # string = fromOk result
    # maybe = fromJSON $ fromString string
    | isNothing maybe = (throw $ ParseError "Could not parse manifest file", world)
    # manifest = fromJust maybe
    = (Ok manifest, world)

readMainManifest :: *World -> *Return Manifest
readMainManifest world
    // # world = logInf ["Reading main manifest file"] world
    = readManifest "." world

showMainManifest :: *World -> *World
// showMainManifest = readMainManifest >>= showManifest
// showMainManifest = showManifest <*> readMainManifest
showMainManifest world
    # (result,world) = readMainManifest world
    | isError result = world
    # manifest = fromOk result
    = showManifest manifest world

/// ## Methods

showManifest :: Manifest *World -> *World
showManifest manifest world
    # world = putAct ["Manifest information for", manifest.info.BasicInfo.name] world
    = putStrLn (pretty manifest) world

writeManifest :: FilePath Manifest *World -> *Return ()
writeManifest path manifest world
    # world = logInf ["Writing manifest file to", quote path] world
    # (result,world) = writeFile path (toString $ toJSON manifest) world
    // putErr ["Could not write manifest to", quote path, ":", toString error] world
    | isError result = (rethrow (FileError path) result, world)//FIXME ugly
    = (Ok (), world)

/// ### Helpers

//XXX move elsewhere
replace :: Char Char -> String -> String
replace x y = toString o 'List'.map (\e -> e == x ? y $ e) o fromString

