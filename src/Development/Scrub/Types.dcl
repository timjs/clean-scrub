definition module Development.Scrub.Types

from Data.Func import $
from Data.Result import :: Result, :: Either(..), fromLeft
from Data.Maybe import :: Maybe
from Data.String import class toString(..)

from Text.JSON import generic JSONDecode, generic JSONEncode, :: JSONNode(..)

from System.File import :: FileError
from System.IO import :: IO
from System.OSError import :: OSError, :: OSErrorCode, :: OSErrorMessage

import System.Console.Output

//
// # The `Run` monad
//

// :: Run a :== WriterT [Message] (ExceptT Error (IO)) a
// :: Run a :== ExceptT Error (WriterT [Message] (IO)) a
// :: Run a :== *World -> (([Message], Either Error a), *World)

/// The `Run` monad is essentially an Exception monad stacked on top of the IO
/// monad. Sadly monad transformers in Clean are heavy (because they are boxed)
/// and ugly (because there is no do-notation). Therefore we define this as a
/// type-macro and use let-before syntax to pack and unpack this structure.
// TODO:
//   Replace alle instances of `*World -> Return` with `Run`
//   after new let-before syntax extension is introduced.
//   (Apperently this violates strictness analysis...)
// :: Run a :== *World -> (Either Error a, *World)
:: Return a :== .(Either Error a, .World)

:: Error
    = SystemError OSError
    | FileError FileError
    | LookupError String
    | ParseError String
    | NinjaError String
    // | ...

instance toString Error

//
// # Type casting
//

/// Class to cast arbitrary types to each other
class Cast a b where
    cast :: a -> b

// instance Cast (Either e a) (Either e` a) | Cast e e`

instance Cast OSError Error
instance Cast FileError Error

rethrow` e :== Left $ cast $ fromLeft e

//
// # Type synonyms
//

:: Name :== String
:: Address :== String

:: Version = Version Major Minor Micro
:: Major :== Int
:: Minor :== Int
:: Micro :== Int

:: Author :== String //= Author Name Address

:: VersionConstraint :== String

//
// ## Instances
//

instance toString Version
parseVersion :: String -> Either String Version

derive JSONDecode Version //instance in .icl
derive JSONEncode Version //instance in .icl

derive JSONDecode Error, Either
derive JSONEncode Error, Either

//
// # Logging
//

// logAct, logRes, logErr, logWrn, logInf :: [String] *World -> *World
//NOTE Doesn't work with `?` `$` operators because macro expansion is after sharing inference...

logAct ms w   :== putAct ms w
logRes ms x w :== putRes ms x w
logErr ms w   :== putErr ms w
logWrn ms w   :== putWrn ms w
logInf ms w   :== putInf ms w

// logAct ms w   :== w
// logRes ms x w :== w
// logErr ms w   :== w
// logWrn ms w   :== w
// logInf ms w   :== w

