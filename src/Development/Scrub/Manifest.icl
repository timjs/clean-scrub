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

readManifest :: FilePath *World -> *Return Manifest
readManifest path world
    # world = logInf ["Reading manifest file from", quote path] world
    # (result`,world) = readFile (path </> manifestFilename) world
    // putErr ["Error reading manifest file from", quote path, ":", toString error] world
    | isError result` = (rethrow` result`, world)
    # string = fromOk result`
    # maybe = fromJSON $ fromString string
    | isNothing maybe = (throw $ ParseError "Could not parse manifest file", world)
    # manifest = fromJust maybe
    = (Ok manifest, world)

readMainManifest :: *World -> *Return Manifest
readMainManifest world
    # world = logInf ["Reading main manifest file"] world
    = readManifest "." world

showManifest :: Manifest *World -> *World
showManifest manifest world
    # world = putAct ["Package information for", manifest.package.BasicInfo.name] world
    = putStrLn (jsonPrettyPrint $ toJSON manifest) world

showMainManifest :: *World -> *World
showMainManifest world
    # (result,world) = readMainManifest world
    | isError result = world
    # manifest = fromOk result
    = showManifest manifest world

writeManifest :: FilePath Manifest *World -> *Return ()
writeManifest path manifest world
    # world = logInf ["Writing manifest file to", quote path] world
    # (result`,world) = writeFile path (toString $ toJSON manifest) world
    // putErr ["Could not write manifest to", quote path, ":", toString error] world
    | isError result` = (rethrow` result`, world)//FIXME ugly
    = (Ok (), world)

//
// # Packages
//

createPackage :: DependencyInfo *World -> *Return Package
createPackage info world
    # path = info.DependencyInfo.path
    # (result,world) = readManifest path world
    | isError result = (rethrow result, world)
    # manifest = fromOk result
    # world = logInf ["Creating package info for", quote path] world
    = (Ok { path = path, manifest = manifest }, world)

//
// # Dependencies
//
//XXX make these recursive for dependencies of dependencies

// readDependencies :: Package *World -> ([Package],*World)
// readDependencies package world
//     = mapSt readDependency package.manifest.dependencies world

