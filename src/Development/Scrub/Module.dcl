definition module Development.Scrub.Module

from Data.Either import :: Either
from Data.Set import :: Set
from Data.Map import :: Map

from System.FilePath import :: FilePath

import Development.Scrub.Types

:: Database :== Map Name FilePath //XXX maybe someday `Map Name Module with Module = {name :: String, version :: Version, path :: FilePath, ..}`

showImports :: FilePath *World -> *World
calcImports :: FilePath *World -> (Either String (Set Name), *World)
parseImports :: String -> Either String (Set Name)

showDependencies :: FilePath *World -> *World
calcDependencies :: FilePath Database *World -> (Either String (Set Name), *World)

