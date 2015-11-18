definition module Development.Scrub

from Data.Func import $
from Data.Result import :: Result, :: Either
from Data.Maybe import :: Maybe
from Data.Map import :: Map
from Data.Set import :: Set
from Data.Show import class toString(..)

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

/// ## The `Run` Monad

/// The `Run` monad is essentially an Exception monad stacked on top of the IO
/// monad. Sadly monad transformers in Clean are heavy (because they are boxed)
/// and ugly (because there is no do-notation). Therefore we define this as a
/// type-macro and use let-before syntax to pack and unpack this structure.
// TODO:
//   Replace alle instances of `*World -> Return` with `Run`
//   after new let-before syntax extension is introduced.
//   (Apperently this violates strictness analysis...)
// :: Run a :== WriterT [Message] (ExceptT Error (IO)) a
// :: Run a :== ExceptT Error (WriterT [Message] (IO)) a
// :: Run a :== World -> (([Message], Either Error a),World)
// :: Run a :== *World -> (Either Error a, *World)
:: Return a :== .(Either Error a, .World)

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

/// ## Logging

logAct ms w   :== putAct ms w
logRes ms x w :== putRes ms x w//TODO refactor?
logErr ms w   :== putErr ms w
logWrn ms w   :== putWrn ms w
logInf ms w   :== putInf ms w

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

createPackage :: FilePath *World -> *Return Package
createPackageFromDependency :: DependencyInfo *World -> *Return Package
showMainPackage :: *World -> *World//TODO remove?
showMainModuleDictionary :: *World -> *World//TODO remove?

showPackage :: Package *World -> *World//TODO remove?

showModuleImports :: FilePath *World -> *World//TODO remove?
calculateModuleImports :: FilePath *World -> *Return (Set Name)
parseModuleImports :: String -> Result Error (Set Name)

createModuleDictionary :: Package *World -> *Return Dictionary
addPackageDependency :: DependencyInfo *World -> *Return Dictionary

showModuleDependencies :: Dictionary FilePath *World -> *World
calculateModuleDependencies :: FilePath Dictionary *World -> *Return (Set Name)

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

readManifest :: FilePath *World -> *Return Manifest
readMainManifest :: *World -> *Return Manifest
showMainManifest :: *World -> *World

showManifest :: Manifest *World -> *World
writeManifest :: FilePath Manifest *World -> *Return ()

////////////////////////////////////////////////////////////////////////////////
/// # Modules
////////////////////////////////////////////////////////////////////////////////

// :: Module =
//     { name :: Name
//     , path :: FilePath
//     , imports :: Set Name
//     , dependencies :: Set Name
//     }

// createModule :: FilePath *World -> *Return Module
