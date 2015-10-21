implementation module Development.Scrub.Command

import Data.Func
import Data.Result
import Data.String
import Data.Tuple

import System.File
import System.Console.Output

import Development.Scrub.Manifest
import Development.Scrub.Module

run :: String [String] *World -> *World

run "imports" args world
    = seqSt showImports args world

run "dependencies" args world
    = seqSt showDependencies args world

run "generate" args world
    = undef

run "build" args world
    = undef

run "rebuild" args world
    = undef

run "resolve-module" args world
    = undef

run "info" args world
    = showMainManifest world

run "help" args world
    = putStrLn helpMessage world

run command args world
    = putErr [quote command, " is not a scrub command, see 'scrub help'"] world

helpMessage :== "This is Scrub v0.0.7 by Tim Steenvoorden"

