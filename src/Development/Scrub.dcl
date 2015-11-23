definition module Development.Scrub

from Data.Func import $
from Data.Result import :: Result, :: Either
from Data.Maybe import :: Maybe
from Data.Map import :: Map
from Data.Set import :: Set
from Data.Show import class toString(..)

from Data.Functor import class Functor
from Control.Applicative import class Applicative
from Control.Monad import class Monad

from Text.JSON import generic JSONDecode, generic JSONEncode, :: JSONNode
from Text.JSON import generic JSONDecode, generic JSONEncode, :: JSONNode

from System.File import :: FileError
from System.FilePath import :: FilePath, </>, combine
from System.OSError import :: OSError, :: OSErrorCode, :: OSErrorMessage

import System.Console.Output

////////////////////////////////////////////////////////////////////////////////
/// # Global defaults
////////////////////////////////////////////////////////////////////////////////

manifestFilename :== "Scrub.json" //TODO change to .toml
defaultSourceDir :== "src/"
defaultExecutableFile :== "Main.icl"
defaultExecutable name :== { ExecutableInfo | name = name, main = defaultSourceDir </> defaultExecutableFile }
definitionExtension :== "dcl" //XXX move elsewhere
moduleSeparator :== '.'

////////////////////////////////////////////////////////////////////////////////
/// # Type synonyms
////////////////////////////////////////////////////////////////////////////////

:: Name :== String
:: Address :== String

:: Version = Version Major Minor Micro
:: Major :== Int
:: Minor :== Int
:: Micro :== Int

:: Author :== String //TODO Author Name Address, toString, fromString

:: VersionConstraint :== String

:: Dictionary :== Map Name FilePath

derive JSONDecode Version // instance in implementation
derive JSONEncode Version // instance in implementation

instance toString Version
parseVersion :: String -> Either String Version

////////////////////////////////////////////////////////////////////////////////
/// # Control flow
////////////////////////////////////////////////////////////////////////////////

/// ## The `Throwing` Class

class Throwing m | Monad m where
    throw :: Error -> m a
    // throw :: e -> m a
    // catch :: (m a) (e -> m a) -> m a

/// ## The `Run` Monad

/// The `Run` monad is essentially an Exception monad stacked on top of the IO
/// monad. Sadly monad transformers in Clean are heavy (because they are boxed)
/// and ugly (because there is no do-notation). Therefore we define this as a
/// type-macro and use let-before syntax to use this structure.
// :: Run a :== WriterT [Message] (ExceptT Error (IO)) a
// :: Run a :== ExceptT Error (WriterT [Message] (IO)) a
// :: Run a :== World -> (([Message], Either Error a),World)

:: Run a =: Run (*World -> *(Either Error a, *World))

instance Functor Run
instance Applicative Run
instance Monad Run

execRun :: (Run a) *World -> *World
evalRun :: (Run a) *World -> *(Either Error a, *World)
// mapRun :: ((Either Error a) -> Either Error a) (Run a) -> Run a
withError :: (e -> Error) (*World -> *(Either e a, *World)) -> Run a // liftError ?

withRun :: (*World -> *(Either Error a, *World)) -> Run a
withEither :: (Either Error a) -> Run a // liftEither ?
withVoid :: (*World -> *World) -> Run () // liftWorld ?
withValue :: (*World -> (a, *World)) -> Run a // liftValue ?

/// ## Errors

:: Error
    = SystemError OSError
    | FileError FilePath FileError
    | PackageError Name [Name]
    | LookupError Name

    | ParseError String
    | NinjaError String
    // | ...

instance toString Error

derive JSONDecode Error, Either
derive JSONEncode Error, Either

/// ## Logging and Printing

// put, putAct, putRes, putErr, putWrn, putInf :: [String] -> Run ()
put    s    :== withVoid $ putStrLn s
putAct ms   :== put (green  ">>> " +++ foldSep ms +++ "...")
putRes ms x :== put (blue   "=== " +++ foldSep ms +++ ": " +++ show x)
putErr ms   :== put (red    "!!! " +++ foldSep ms)
putWrn ms   :== put (yellow "*** " +++ foldSep ms)
putInf ms   :== put (white  "... " +++ foldSep ms)

logAct ms   :== putAct ms
logRes ms x :== putRes ms x//TODO refactor?
logErr ms   :== putErr ms
logWrn ms   :== putWrn ms
logInf ms   :== putInf ms

// logAct ms w   :== w
// logRes ms x w :== w//TODO refactor?
// logErr ms w   :== w
// logWrn ms w   :== w
// logInf ms w   :== w

////////////////////////////////////////////////////////////////////////////////
/// # Packages
////////////////////////////////////////////////////////////////////////////////

:: Package =
    { name :: Name
    , version :: Version
    , authors :: [Author]
    , path :: FilePath
    , sourceDirs :: [FilePath]
    , dependencies :: [DependencyInfo] //Map Name (Either VersionConstraint DependencyInfo)
    , executables :: [ExecutableInfo]
    , localModules :: Map Name FilePath //XXX Set Name ???
    , exportedModules :: Map Name FilePath //XXX
    }

derive JSONEncode Package
derive JSONDecode Package

createPackage :: FilePath -> Run Package
createPackageFromDependency :: DependencyInfo -> Run Package
showMainPackage :: Run () //TODO remove?
showMainModuleDictionary :: Run () //TODO remove?

showPackage :: Package -> Run () //TODO remove?

showModuleImports :: FilePath -> Run () //TODO remove?
calculateModuleImports :: FilePath -> Run (Set Name)
parseModuleImports :: String -> Either Error (Set Name)

createModuleDictionary :: Package -> Run Dictionary
addPackageDependency :: DependencyInfo -> Run Dictionary
showModuleDictionary :: Dictionary -> Run ()

showModuleDependencies :: Dictionary FilePath -> Run ()
calculateModuleDependencies :: FilePath Dictionary -> Run (Set Name)


////////////////////////////////////////////////////////////////////////////////
/// # Manifest
////////////////////////////////////////////////////////////////////////////////

:: Manifest =
    { info :: BasicInfo
    , dependencies :: Maybe [DependencyInfo] //Map Name (Either VersionConstraint DependencyInfo)
    , executables :: Maybe [ExecutableInfo]
    , library :: Maybe LibraryInfo
    }

:: BasicInfo =
    { name :: Name
    , version :: Version
    , authors :: [Author]
    , sourceDirs :: Maybe [FilePath]
    }
:: DependencyInfo =
    { name :: Name
    , version :: VersionConstraint //TODO how do we check versions?
    , path :: FilePath
    //, git :: Maybe FilePath
    //TODO more to come
    }
:: ExecutableInfo =
    { name :: Name
    , main :: FilePath
    //TODO more to come?
    }
:: LibraryInfo =
    { name :: Maybe Name
    , modules :: [Name]
    //TODO more to come?
    }

derive JSONDecode Manifest
derive JSONEncode Manifest

readManifest :: FilePath -> Run Manifest
readMainManifest :: Run Manifest
showMainManifest :: Run ()

showManifest :: Manifest -> Run ()
writeManifest :: FilePath Manifest -> Run ()

////////////////////////////////////////////////////////////////////////////////
/// # Modules
////////////////////////////////////////////////////////////////////////////////

// :: Module =
//     { name :: Name
//     , path :: FilePath
//     , imports :: Set Name
//     , dependencies :: Set Name
//     }

// createModule :: FilePath -> Run Module
