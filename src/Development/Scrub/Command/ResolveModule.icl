implementation module Development.Scrub.Command.ResolveModule

import Data.Func

import System.FilePath
import System.Console.Output

import Development.Scrub.Types
import Development.Scrub.Manifest

run :: [Name] *World -> *World
run args world
    = undef
    // = seqSt showPathOf args world

// showPathOf :: Name *World -> *World
// showPathOf module world
//     # world = putAct ["Resolving path for ", quote module] world
//     # (result,world) = pathOf module world
//     = world

// pathOf :: Name Database -> Maybe FilePath
// pathOf module database = lookup module database

