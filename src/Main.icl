module Main

import Data.Func
import Data.List
import Data.Tuple

import System.CommandLine
import System.Console.Output
import System.Process

import Development.Scrub.Command
import Development.Scrub.Types

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

