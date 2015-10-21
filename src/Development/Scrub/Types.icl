implementation module Development.Scrub.Types

import Data.Eq
import Data.Func
import Data.Maybe
import Data.Result
import Data.String

import Data.Functor
import Data.Foldable

from Text.JSON import generic JSONDecode, generic JSONEncode, :: JSONNode(..)

import System.OSError
import System.File

import Development.Scrub.Parsers

//
// # Version
// 

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

derive JSONDecode Error, Either, FileError
derive JSONEncode Error, Either, FileError

//
// # Casts
//

// instance Cast (Either e a) (Either e` a) | Cast e e` where
//     cast (Left e) = Left (cast e)
//     cast (Right a) = Right a

instance Cast OSError Error where
    cast e = SystemError e
instance Cast FileError Error where
    cast e = FileError e

//TODO improve error messages
instance toString Error where
    toString (SystemError (error, message))
        = "A system error occured: (E" +++ toString error +++ ") " +++ message
    toString (FileError error)
        = "File error: " +++ toString error
    toString (ParseError message)
        = "Parse error: " +++ message
    toString (LookupError message)
        = "Lookup error: " +++ message
    toString (NinjaError message)
        = "Ninja error: " +++ message

