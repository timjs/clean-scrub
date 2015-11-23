module Main

/// Scrub is a ...
///
/// Shortcommings:
/// * Does not resolve dependencies of dependencies yet

import Base

// import Data.List

import System.CommandLine
import System.File
import System.Process

import System.Console.Output
import Development.Scrub

////////////////////////////////////////////////////////////////////////////////
/// # Command line interface
////////////////////////////////////////////////////////////////////////////////

run :: !String [String] -> Run ()

run "imports" args =
    traverse_ showModuleImports args

run "modules" [] =
    showMainModuleDictionary

run "modules" args =
    // traverse_ (createPackage >=> createModuleDictionary >=> showModuleDictionary) args
    traverse createPackage args >>= traverse createModuleDictionary >>= traverse_ showModuleDictionary

run "dependencies" args =
    createPackage "." >>=
    createModuleDictionary >>= \dictionary ->
    traverse_ (showModuleDependencies dictionary) args

run "generate" args =
    undef

run "build" args =
    undef

run "rebuild" args =
    undef

run "resolve-module" args =
    undef

run "info" [] =
    showMainPackage

run "info" args =
    traverse createPackage args >>=
    traverse_ showPackage

run "help" args =
    put helpMessage

run command args =
    putErr [quote command, " is not a scrub command, see 'scrub help'"]

helpMessage :== "This is Scrub v0.0.13 by Tim Steenvoorden"

////////////////////////////////////////////////////////////////////////////////
/// # Start
////////////////////////////////////////////////////////////////////////////////

Start :: *World -> *World
Start world
    # ([_:args],world) = getCommandLine world
    # (command,args) = null args ? ("help", []) $ (head args, tail args)
    # (result,world) = evalRun (run command args) world
    = case result of
        Left e -> putStrLn (toString e) world
        // Right a -> putStrLn (toString a) world
        Right a -> world
    // = case result of
    //     Ok _
    //         = snd $ exit 0 world
    //     Error e
    //         # world = putErr [toString e] world
    //         = snd $ exit 1 world

