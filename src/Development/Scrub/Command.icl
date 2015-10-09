implementation module Development.Scrub.Command

import Data.Func
import Data.String

import System.File
import System.Console.Output

import Development.Scrub.Manifest
import Development.Scrub.Module

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

run "deps" args world
    = seqSt showImportsOf args world

run "generate" args world
    = undef

run "build" args world
    = undef

run "rebuild" args world
    = undef

run "resolve-module" args world
    = undef

run "info" args world
    # (manifest,world) = readManifest world
    = showManifest manifest world

run "help" args world
    = putStrLn helpMessage world

run command args world
    = putErr [quote command, " is not a scrub command, see 'scrub help'"] world

helpMessage :== "This is Scrub v0.0.1 by Tim Steenvoorden"

