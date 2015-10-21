definition module Development.Scrub.Module

from Data.Result import :: Result, :: Either
from Data.Set import :: Set
from Data.Map import :: Map

from System.FilePath import :: FilePath

import Development.Scrub.Types

:: Database :== Map Name FilePath //XXX maybe someday `Map Name Module with Module = {name :: String, version :: Version, path :: FilePath, ..}`

showImports :: FilePath *World -> *World
calcImports :: FilePath *World -> *Return (Set Name)
parseImports :: String -> Result Error (Set Name)

showDependencies :: FilePath *World -> *World
calcDependencies :: FilePath Database *World -> *Return (Set Name)

