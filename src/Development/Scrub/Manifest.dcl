definition module Development.Scrub.Manifest

from Data.Either import :: Either
from Data.Maybe import :: Maybe
from Data.Map import :: Map

from Text.JSON import generic JSONDecode, generic JSONEncode, :: JSONNode

from System.FilePath import :: FilePath

import Development.Scrub.Types

manifestFilename :== "Scrub.json" //FIXME change to .toml

:: Package = //XXX other name so we dont have to write package.manifest.package.name
    { path :: FilePath
    , manifest :: Manifest
    }

:: Manifest =
    { package :: BasicInfo
    , dependencies :: [DependencyInfo] //Map Name (Either VersionConstraint DependencyInfo)
    , executables :: Maybe [ExecutableInfo]
    , library :: Maybe LibraryInfo
    }

:: BasicInfo =
    { name :: Name
    , version :: Version
    , authors :: [Author]
    , sources :: Maybe FilePath
    }
:: DependencyInfo =
    { name :: Name
    , version :: VersionConstraint //XXX how do we check versions?
    , path :: FilePath
    //, git :: Maybe FilePath
    //XXX more to come
    }
:: ExecutableInfo =
    { name :: Name
    , main :: FilePath
    //XXX more to come?
    }
:: LibraryInfo =
    { name :: Maybe Name
    , modules :: [Name]
    //XXX more to come?
    }

derive JSONDecode Manifest
derive JSONEncode Manifest

readManifest :: FilePath *World -> *Return Manifest
readMainManifest :: *World -> *Return Manifest
showManifest :: Manifest *World -> *World
showMainManifest :: *World -> *World
writeManifest :: FilePath Manifest *World -> *Return ()

createPackage :: DependencyInfo *World -> *Return Package

