implementation module Development.Scrub.Manifest

import Data.Func
import Override.Data.Either
import Data.Tuple

import qualified Data.Map as Map
from Data.Map import :: Map

import Text.JSON

import System.Console.Output
import System.File
import System.FilePath
import System.Process

import Development.Scrub.Types

derive JSONDecode Manifest, PackageInfo, DependencieInfo, LibraryInfo, MaybeError
derive JSONEncode Manifest, PackageInfo, DependencieInfo, LibraryInfo, MaybeError

readManifest :: *World -> (Manifest,*World)
readManifest world
    # world = putAct ["Reading manifest file"] world
    # (result,world) = readFile MANIFEST_FILENAME world
    = case result of
        Left error
            # world = putErr ["Could not read ", quote MANIFEST_FILENAME, ": ", toString error] world
            = exit 1 world
        Right string
            # json = fromString string
            = case fromJSON json of
                Nothing
                    # world = putErr ["Could not parse ", quote MANIFEST_FILENAME] world
                    = exit 1 world
                Just manifest = (manifest, world)

showManifest :: Manifest *World -> *World
showManifest manifest world
    # world = putInf ["Current manifest:"] world
    = putStrLn (toString $ toJSON manifest) world

writeManifest :: FilePath Manifest *World -> *World
writeManifest path manifest world
    # world = putAct ["Writing manifest file to ", quote path] world
    # (result,world) = writeFile path (toString $ toJSON manifest) world
    = case result of
        Left error
            # world = putErr ["Could not write manifest to ", quote path, ": ", toString error] world
            = snd $ exit 1 world
        Right _ = world

