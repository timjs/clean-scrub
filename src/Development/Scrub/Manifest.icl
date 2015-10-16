implementation module Development.Scrub.Manifest

import Data.Func
import Data.Either
import Data.Tuple

import qualified Data.Map as Map
from Data.Map import :: Map

import Text.JSON

import System.Console.Output
import System.File
import System.FilePath
import System.Process

import Development.Scrub.Types

derive JSONDecode Manifest, BasicInfo, DependencyInfo, LibraryInfo, ExecutableInfo
derive JSONEncode Manifest, BasicInfo, DependencyInfo, LibraryInfo, ExecutableInfo

//
// # Manifest files
//

readManifest :: FilePath *World -> (Manifest,*World)
readManifest path world
    # (result,world) = traceAct ["Reading manifest file from", quote path] $
        readFile (path </> manifestFilename) world
    = case result of
        Left error
            # world = putErr ["Error reading manifest file from", quote path, ":", toString error] world
            = exit 1 world
        Right string
            # json = fromString string
            = case fromJSON json of
                Nothing
                    # world = putErr ["Error parsing manifest file from", quote path] world
                    = exit 1 world
                Just manifest = (manifest, world)

readMainManifest :: *World -> (Manifest,*World)
readMainManifest world
    = traceAct ["Reading main manifest file"] $ readManifest "." world

showManifest :: Manifest *World -> *World
showManifest manifest world
    # world = putInf ["Current manifest:"] world
    = putStrLn (toString $ toJSON manifest) world

writeManifest :: FilePath Manifest *World -> *World
writeManifest path manifest world
    # (result,world) = traceAct ["Writing manifest file to", quote path] $
        writeFile path (toString $ toJSON manifest) world
    = case result of
        Left error
            # world = putErr ["Could not write manifest to", quote path, ":", toString error] world
            = snd $ exit 1 world
        Right _ = world

//
// # Packages
//

createPackage :: DependencyInfo *World -> (Package, *World)
createPackage info world
    # path = info.DependencyInfo.path
    # (manifest,world) = readManifest path world
    = traceAct ["Creating package info for", quote path] $
        ({ path = path, manifest = manifest }, world)

//
// # Dependencies
//
//XXX make these recursive for dependencies of dependencies

// readDependencies :: Package *World -> ([Package],*World)
// readDependencies package world
//     = mapSt readDependency package.manifest.dependencies world

