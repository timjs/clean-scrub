implementation module Development.Scrub

import Base

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

import Text.Femtoparsec
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
/// # Control Flow
////////////////////////////////////////////////////////////////////////////////

/// ## The `Run` Monad

instance Functor Run where
    fmap a2b (Run fa) = Run (\w
        # (ea,w) = fa w
        -> case ea of
            Left e -> (Left e, w)
            Right a -> (Right $ a2b a, w))

instance Applicative Run where
    pure a = Run (\w -> (Right a, w))

    (<*>) (Run fa2b) (Run fa) = Run (\w
        # (ea2b,w) = fa2b w
        -> case ea2b of
            Left e -> (Left e, w)
            Right a2b
                # (ea,w) = fa w
                -> case ea of
                    Left e -> (Left e, w)
                    Right a -> (Right $ a2b a, w))

instance Monad Run where
    bind (Run ma) a2mb = Run (\w
        # (ea,w) = ma w
        -> case ea of
            Left e -> (Left e, w)
            Right a
                # (Run mb) = a2mb a
                -> mb w)

instance Throwing Run where
    throw e = Run (\w -> (Left e, w))

execRun :: (Run a) *World -> *World
execRun (Run ma) world
  # (_, world) = ma world
  = world

evalRun :: (Run a) *World -> *(Either Error a, *World)
evalRun (Run ma) world = ma world

mapRun :: ((Either Error a) -> Either Error a) (Run a) -> Run a
mapRun f (Run ma) = Run (\world
    # (ea,world) = ma world
    -> (f ea, world))

withRun :: (*World -> *(Either Error a, *World)) -> Run a
withRun f = Run f

withEither :: (Either Error a) -> Run a
withEither ea = Run (\world -> (ea, world))

withVoid :: (*World -> *World) -> Run ()
withVoid f = Run (\world
    # world = f world
    = (Right (), world))

withValue :: (*World -> (a, *World)) -> Run a
withValue f = Run (\world
    # (a,world) = f world
    = (Right a, world))

withError :: (e -> Error) (*World -> *(Either e a, *World)) -> Run a
// withError f g = mapRun (mapLeft f) $ withRun g
withError f g = Run (\world
    # (ea,world) = g world
    = case ea of
        Left e -> (Left $ f e, world)
        Right a -> (Right a, world))

// mapWorld :: ((Either e a) -> Either e` a) (*World -> *(Either e a, *World)) -> Run e` a
// mapWorld f g = mapRun f $ withRun g

/// ## Errors

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
    toString (PackageError name modules)
        = "Package " +++ quote name +++ " exports modules without implementation: " +++ show modules
    toString (LookupError module)
        = "Could not find module " +++ quote module +++ " in packages"
    toString (ParseError message)
        = "Parse error: " +++ message
    toString (NinjaError message)
        = "Ninja error: " +++ message

////////////////////////////////////////////////////////////////////////////////
/// # Packages
////////////////////////////////////////////////////////////////////////////////

derive JSONEncode Package
derive JSONDecode Package

/// ## Initialisers

createPackage :: FilePath -> Run Package
createPackage path =
    logInf ["Creating package info for", quote path] >>|
    readManifest path >>= \manifest ->
    let name = manifest.info.BasicInfo.name in
    let modules = maybe [] (\lib -> lib.LibraryInfo.modules) manifest.library in
    logRes ["Exported modules from", quote name] modules >>|
    let dirs = maybe [defaultSourceDir] id manifest.info.BasicInfo.sourceDirs in
    let sourceDirs = 'List'.map (combine path) dirs in
    logRes ["Source directories for", quote name] sourceDirs >>|
    traverse findLocalModules sourceDirs >>= \dictionaries ->
    let locals = 'Map'.unions $ dictionaries in
    let exports = 'Map'.fromList $ 'List'.zip2 modules ('List'.repeat "") in
    let missing = 'Map'.difference exports locals in
    if (not $ 'Map'.null missing)
        (throw $ PackageError manifest.info.BasicInfo.name ('Map'.keys missing))
        (return { Package
            | name = name
            , version = manifest.info.BasicInfo.version
            , authors = manifest.info.BasicInfo.authors
            , path = path
            , sourceDirs = sourceDirs
            , dependencies = maybe [] id manifest.Manifest.dependencies //TODO add clean-base as implicit dependencie for every package?
            , executables = maybe [defaultExecutable manifest.info.BasicInfo.name] id manifest.Manifest.executables
            , localModules = locals
            , exportedModules = 'Map'.intersection locals exports
            // , dictionary = locals \/ exports /\ exports // Union on Maps is left biased! Exported modules without a .dcl now have an empty path.
            })

createPackageFromDependency :: DependencyInfo -> Run Package
createPackageFromDependency info = createPackage info.DependencyInfo.path //TODO change to dir in registry

showMainPackage :: Run ()
showMainPackage = createPackage "." >>= showPackage

showMainModuleDictionary :: Run ()
showMainModuleDictionary = createPackage "." >>= createModuleDictionary >>= showModuleDictionary

/// ## Methods

showPackage :: Package -> Run ()
showPackage package =
    putAct ["Package information for", package.Package.name] >>|
    put (pretty $ package)

/// ### Module Imports

showModuleImports :: FilePath -> Run ()
showModuleImports path =
    putAct ["Calculating imports of", quote path] >>|
    calculateModuleImports path >>= \imports ->
    traverse_ put ('Set'.toList imports)

calculateModuleImports :: FilePath -> Run (Set Name)
calculateModuleImports path =
    logInf ["Reading contents of", quote path] >>|
    readFile path |> withError (FileError path) >>= \string ->
    parseModuleImports string |> withEither

parseModuleImports :: String -> Either Error (Set Name)
parseModuleImports string = mapBoth ParseError 'Set'.fromList $ parseOnly imports string

/// ### Module Database

createModuleDictionary :: Package -> Run Dictionary
createModuleDictionary package =
    logInf ["Creating module dictionary"] >>|
    traverse addPackageDependency package.Package.dependencies >>= \others ->
    let dictionary = 'List'.foldr 'Map'.union package.localModules others in
    return dictionary

showModuleDictionary :: Dictionary -> Run ()
showModuleDictionary dictionary = put (pretty $ 'Map'.toList dictionary)

addPackageDependency :: DependencyInfo -> Run Dictionary
addPackageDependency dependency =
    logInf ["Adding exported modules from", dependency.DependencyInfo.name, "version", dependency.DependencyInfo.version] >>|
    //TODO someday resolve by name and version
    findPackage dependency.DependencyInfo.path >>= \package ->
    let modules = package.localModules in
    logRes ["Added modules from", quote dependency.DependencyInfo.name] modules >>|
    return modules

/// ### Module Dependencies

showModuleDependencies :: Dictionary FilePath -> Run ()
showModuleDependencies dictionary path =
    putAct ["Calculating dependencies of", quote path] >>|
    calculateModuleDependencies path dictionary >>= \dependencies ->
    traverse_ put ('Set'.toList dependencies)

calculateModuleDependencies :: FilePath Dictionary -> Run (Set Name)
calculateModuleDependencies path dictionary =
    calculateModuleImports path >>= \todo ->
    go todo 'Set'.empty
    where
        // go :: (Set a) (Set a) -> Run (Set Name)
        go todo done
            | 'Set'.null todo = return done
            | otherwise =
                let (current,rest) = 'Set'.deleteFindMin todo in
                lookupModule current dictionary |> withEither >>= \path ->
                calculateModuleImports path >>= \imports ->
                let todo` = rest \/ imports \\\ done in
                let done` = 'Set'.insert current done in
                go todo` done`

lookupModule :: Name Dictionary -> Result Error FilePath
lookupModule module dictionary
    = case 'Map'.lookup module dictionary of
        Nothing -> Error $ LookupError module
        Just path -> Ok path

/// ## Helpers

//TODO someday :: Name Version -> Run Package
findPackage :: FilePath -> Run Package
findPackage path =
    logInf ["Looking up package in", quote path] >>|
    // # world = logInf ["Looking up package", name, ", version:", version] world
    createPackage path

findLocalModules :: FilePath -> Run Dictionary
findLocalModules sourceDir =
    logInf ["Looking up local modules in", quote sourceDir] >>|
    findFiles definitionPredicate sourceDir |> withError SystemError >>= \definitionPaths ->
    let moduleNames = 'List'.map translate definitionPaths in
    logRes ["Found local modules"] moduleNames >>|
    'Map'.fromList ('List'.zip2 moduleNames definitionPaths) |> return
    where
        translate = replace pathSeparator moduleSeparator o makeRelative sourceDir o dropExtension
        definitionPredicate info = takeExtension info.FileInformation.path == definitionExtension

/// ### Import parser

isName :: Char -> Bool
isName c = isAlphaNum c || c == '_' || c == '`' || c == '.'

name :: Parser Name
name = takeWhile1 isName

names :: Parser [Name]
names = sepBy1 name (char ',' *> skipHorizontalSpace1)

importLine :: Parser [Name]
importLine = string "import" *> skipHorizontalSpace1 *> optional (string "qualified" *> skipHorizontalSpace1) *> names <* restOfLine

fromLine :: Parser [Name]
fromLine = (\x -> [x]) <$> (string "from" *> skipHorizontalSpace1 *> name <* restOfLine)

otherLine :: Parser [Name]
otherLine = restOfLine *> pure []

imports :: Parser [Name]
imports = 'List'.flatten <$> many (importLine <|> fromLine <|> otherLine)

////////////////////////////////////////////////////////////////////////////////
/// # Manifest
////////////////////////////////////////////////////////////////////////////////

derive JSONDecode Manifest, BasicInfo, DependencyInfo, LibraryInfo, ExecutableInfo
derive JSONEncode Manifest, BasicInfo, DependencyInfo, LibraryInfo, ExecutableInfo

/// ## Initialisers

readManifest :: FilePath -> Run Manifest
// readManifest path = ... fromJSON $ fromString <$> readFile (path </> manifestFilename)
readManifest path =
    logInf ["Reading manifest file from", quote path] >>|
    readFile (path </> manifestFilename) |> withError (FileError path) >>= \string ->
    // putErr ["Error reading manifest file from", quote path, ":", toString error] >>|
    case fromJSON $ fromString string of
        Nothing -> throw $ ParseError "Could not parse manifest file"
        Just manifest -> return manifest

readMainManifest :: Run Manifest
readMainManifest = readManifest "."
    // # world = logInf ["Reading main manifest file"] world

showMainManifest :: Run ()
showMainManifest = readMainManifest >>= showManifest

/// ## Methods

showManifest :: Manifest -> Run ()
showManifest manifest =
    putAct ["Manifest information for", manifest.info.BasicInfo.name] >>|
    put (pretty manifest)

writeManifest :: FilePath Manifest -> Run ()
writeManifest path manifest =
    logInf ["Writing manifest file to", quote path] >>|
    writeFile path (toString $ toJSON manifest) |> withError (FileError path)
    // putErr ["Could not write manifest to", quote path, ":", toString error] world

/// ### Helpers

//XXX move elsewhere
replace :: Char Char -> String -> String
replace x y = toString o 'List'.map (\e -> e == x ? y $ e) o fromString

