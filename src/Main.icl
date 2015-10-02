module Main

import Data.Func
import Data.List

import System.CommandLine
import System.Process

import qualified Development.Scrub.Command

Start :: *World -> *World
Start world
    # ([progname:args],world) = getCommandLine world
    | null args = 'Development.Scrub.Command'.run "help" args world
    # [command:args] = args
    = 'Development.Scrub.Command'.run command args world

