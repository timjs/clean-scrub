definition module Development.Scrub.Types

from Data.Either import :: Either
from Data.Maybe import :: Maybe
from Data.String import class toString(..)

from Text.JSON import generic JSONDecode, generic JSONEncode, :: JSONNode(..)

:: Name :== String
:: Address :== String

:: Version = Version Major Minor Micro // major, minor, micro
:: Major :== Int
:: Minor :== Int
:: Micro :== Int

:: Author :== String //= Author Name Address

:: VersionConstraint :== String

instance toString Version
parseVersion :: String -> Either String Version

derive JSONDecode Version //instance in .icl
derive JSONEncode Version //instance in .icl

