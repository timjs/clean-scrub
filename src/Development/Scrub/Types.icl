implementation module Development.Scrub.Types

import Data.Eq
import Data.Functor
import Data.Foldable

import Data.Func
from Data.String import class toString(..)
from Data.Maybe import :: Maybe(..)

from Text.JSON import generic JSONDecode, generic JSONEncode, :: JSONNode(..)

from Text.Parsers.Parsers import number, :: Parser

import Development.Scrub.Parsers

parseVersion :: String -> Either String Version
parseVersion input = parseOnly parser input
    where
        parser = Version <$> decimal <* char '.' <*> decimal <* char '.' <*> decimal

instance toString Version where
    toString (Version x y z) = toString x +++ "." +++ toString y +++ "." +++ toString z

// JSONDecode{|Version|} :: !Bool ![JSONNode] -> (!Maybe Version,![JSONNode])
JSONDecode{|Version|} _ nodes=:[JSONString candidate : rest]
    = case parseVersion candidate of
        Left _ = (Nothing, nodes)
        Right version = (Just version, rest)
JSONDecode{|Version|} _ nodes
    = (Nothing, nodes)

// JSONEncode{|Version|} :: !Bool !Version -> [JSONNode]
JSONEncode{|Version|} _ version
    = [JSONString $ toString version]

