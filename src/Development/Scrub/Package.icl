implementation module Development.Scrub.Package

import Development.Scrub.Manifest

:: Package =
    { name :: Name
    , version :: Version
    , authors :: [Author]
    , path :: FilePath
    , moduleDatabase :: ModuleDatabase
    }

