implementation module Development.Scrub.Command.Deps

import Data.Func
import Override.Data.Either
import Data.Set

import Control.Monad

import System.FilePath
import System.File

import Development.Scrub.Types

run :: [String] *World -> *World
run args world = undef

importsIn :: String -> Either e (Set Name)
importsIn text = undef

importsOf :: FilePath *World -> (Either FileError (Set Name), *World)
importsOf path world
    # (result,world) = readFile path world
    = (result >>= importsIn, world)

// importsOf :: FilePath *World -> (Either FileError (Set Name), *World)
// importsOf path world
//     = let (result,world) = readFile path world in
//         (result >>= importsIn, world)

