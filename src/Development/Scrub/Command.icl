implementation module Development.Scrub.Command

import Data.String

import System.File
import System.Console.Output

import Development.Scrub.Manifest

import qualified Development.Scrub.Command.Deps
import qualified Development.Scrub.Command.Generate
import qualified Development.Scrub.Command.Build
import qualified Development.Scrub.Command.Rebuild
import qualified Development.Scrub.Command.ResolveModule

instance fromString Command where
    fromString "help" = Help
    fromString "deps" = Deps
    fromString "generate" = Generate
    fromString "build" = Build
    fromString "rebuild" = Rebuild
    fromString "resolve-module" = ResolveModule
    fromString "info" = Info

instance toString Command where
    toString Help = "help"
    toString Deps = "deps"
    toString Generate = "generate"
    toString Build = "build"
    toString Rebuild = "rebuild"
    toString ResolveModule = "resolve-module"
    toString Info = "info"

run :: String [String] *World -> *World
run "deps" args world = 'Development.Scrub.Command.Deps'.run args world
run "generate" args world = 'Development.Scrub.Command.Generate'.run args world
run "build" args world = 'Development.Scrub.Command.Build'.run args world
run "rebuild" args world = 'Development.Scrub.Command.Rebuild'.run args world
run "resolve-module" args world = 'Development.Scrub.Command.ResolveModule'.run args world
run "info" args world = info args world
run "help" args world = putStrLn helpMessage world
run command args world = putErr [quote command, " is not a scrub command, see 'scrub help'"] world

helpMessage :== "This is Scrub v0.0.1 by Tim Steenvoorden"

info :: [String] *World -> *World
info args world
    # (manifest,world) = readManifest world
    = showManifest manifest world

