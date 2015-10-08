definition module Development.Scrub.Manifest

from Data.Either import :: Either
from Data.Maybe import :: Maybe
from Data.Map import :: Map

from Text.JSON import generic JSONDecode, generic JSONEncode, :: JSONNode

from System.FilePath import :: FilePath

import Development.Scrub.Types

MANIFEST_FILENAME :== "Scrub.json" //FIXME change to .toml

:: Manifest =
    { package :: PackageInfo
    , dependencies :: [DependencieInfo] //Map Name (Either VersionConstraint DependencieInfo)
    , library :: Maybe LibraryInfo
    }

:: PackageInfo =
    { name :: Name
    , version :: Version
    , authors :: [Author]
    }
:: DependencieInfo =
    { name :: Name
    , version :: VersionConstraint
    , path :: Maybe FilePath
    , git :: Maybe FilePath
    //XXX more to come
    }
:: LibraryInfo =
    { name :: Maybe Name
    , modules :: [Name]
    //XXX more to come?
    }

derive JSONDecode Manifest
derive JSONEncode Manifest

readManifest :: *World -> (Manifest,*World)
showManifest :: Manifest *World -> *World
writeManifest :: FilePath Manifest *World -> *World

