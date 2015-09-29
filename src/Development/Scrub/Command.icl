implementation module Development.Scrub.Command

import Data.String

import System.File

import qualified Development.Scrub.Command.Deps
import qualified Development.Scrub.Command.Generate
import qualified Development.Scrub.Command.Build
import qualified Development.Scrub.Command.Rebuild

instance fromString Command where
    fromString "help" = Help
    fromString "deps" = Deps
    fromString "generate" = Generate
    fromString "build" = Build
    fromString "rebuild" = Rebuild

instance toString Command where
    toString Help = "help"
    toString Deps = "deps"
    toString Generate = "generate"
    toString Build = "build"
    toString Rebuild = "rebuild"

run :: String [String] *World -> *World
run "deps" args world = 'Development.Scrub.Command.Deps'.run args world
run "generate" args world = 'Development.Scrub.Command.Generate'.run args world
run "build" args world = 'Development.Scrub.Command.Build'.run args world
run "rebuild" args world = 'Development.Scrub.Command.Rebuild'.run args world
run _ args world = print helpMessage world

helpMessage :== "This is Scrub v0.0.1 by Tim Steenvoorden"

