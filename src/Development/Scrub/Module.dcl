definition module Development.Scrub.Module

from Data.Either import :: Either
from Data.Set import :: Set

from System.FilePath import :: FilePath

import Development.Scrub.Types

showImportsOf :: FilePath *World -> *World
importsOf :: FilePath *World -> (Either String (Set Name), *World)
importsIn :: String -> Either String (Set Name)

