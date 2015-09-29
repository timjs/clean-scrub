definition module Development.Scrub.Command

from Data.String import class toString, class fromString

:: Command
    = Help
    | Generate
    | Build
    | Rebuild
    | Deps
    //XXX more to come...

instance fromString Command
instance toString Command

run :: String [String] *World -> *World

