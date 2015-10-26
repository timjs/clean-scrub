module Main

/// Scrub is a ...
///
/// Shortcommings:
/// * Does not resolve dependencies of dependencies yet

import Data.Func
import Data.List
import Data.Result
import Data.String
import Data.Tuple

import Data.Traversable

import System.CommandLine
import System.Console.Output
import System.File
import System.Process

import Development.Scrub

////////////////////////////////////////////////////////////////////////////////
/// # Command line interface
////////////////////////////////////////////////////////////////////////////////

run :: !String [String] *World -> *World

run "imports" args world
    = seqSt showModuleImports args world

run "modules" args world
    = showMainModuleDictionary world
    //TODO when implemented package registry
    // | null args = showMainModuleDictionary world
    // # (results,world) = mapSt createPackage args world
    // # result = sequence results
    // | isError result = putErr [toString $ fromError result] world
    // # packages = fromOk result
    // = seqSt showModuleDictionary packages world

run "dependencies" args world
    # (result,world) = createPackage "." world
    | isError result = putErr [toString $ fromError result] world
    # package = fromOk result
    # (result,world) = createModuleDictionary package world
    | isError result = putErr [toString $ fromError result] world
    # dictionary = fromOk result
    = seqSt (showModuleDependencies dictionary) args world

run "generate" args world
    = undef

run "build" args world
    = undef

run "rebuild" args world
    = undef

run "resolve-module" args world
    = undef

run "info" args world
    | null args = showMainPackage world
    # (results,world) = mapSt createPackage args world
    # result = sequence results
    | isError result = putErr [toString $ fromError result] world
    # packages = fromOk result
    = seqSt showPackage packages world

run "help" args world
    = putStrLn helpMessage world

run command args world
    = putErr [quote command, " is not a scrub command, see 'scrub help'"] world

helpMessage :== "This is Scrub v0.0.7 by Tim Steenvoorden"

////////////////////////////////////////////////////////////////////////////////
/// # Start
////////////////////////////////////////////////////////////////////////////////

Start :: *World -> *World
Start world
    # ([_:args],world) = getCommandLine world
    # (command,args) = null args ? ("help", []) $ (head args, tail args)
    = run command args world
    // # (result,world) = run command args world
    // = case result of
    //     Ok _
    //         = snd $ exit 0 world
    //     Error e
    //         # world = putErr [toString e] world
    //         = snd $ exit 1 world
