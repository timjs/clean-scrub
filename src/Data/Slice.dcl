definition module Data.Slice

from Data.Eq import class ==
from Data.Ord import class <

from Data.Maybe import :: Maybe

from StdOverloaded import class %

////////////////////////////////////////////////////////////////////////////////
/// # Data Type
////////////////////////////////////////////////////////////////////////////////

:: Slice

instance == Slice
instance < Slice
instance % Slice

////////////////////////////////////////////////////////////////////////////////
/// # Primitives
////////////////////////////////////////////////////////////////////////////////

empty :: Slice
singleton :: !Char -> Slice
slice :: !Int !Int !String -> Slice
wrap :: !String -> Slice
unwrap :: !Slice -> String

////////////////////////////////////////////////////////////////////////////////
/// # Basics
////////////////////////////////////////////////////////////////////////////////

head :: !Slice -> Maybe Char
tail :: !Slice -> Maybe Slice
init :: !Slice -> Maybe Slice
last :: !Slice -> Maybe Char
uncons :: !Slice -> Maybe (Char,Slice)
unsnoc :: !Slice -> Maybe (Slice,Char)

unsafeHead :: !Slice -> Char
unsafeTail :: !Slice -> Slice
unsafeInit :: !Slice -> Slice
unsafeLast :: !Slice -> Char
unsafeUncons :: !Slice -> (Char,Slice)
unsafeUnsnoc :: !Slice -> (Slice,Char)

////////////////////////////////////////////////////////////////////////////////
/// # Sliceable
////////////////////////////////////////////////////////////////////////////////

take :: !Int !Slice -> Slice
drop :: !Int !Slice -> Slice
splitAt :: !Int !Slice -> (Slice, Slice)

takeTill :: !(Char -> Bool) !Slice -> Slice
dropTill :: !(Char -> Bool) !Slice -> Slice
break :: !(Char -> Bool) !Slice -> (Slice, Slice)

takeWhile :: !(Char -> Bool) !Slice -> Slice
dropWhile :: !(Char -> Bool) !Slice -> Slice
span :: !(Char -> Bool) !Slice -> (Slice, Slice)

isPrefixOf :: !String !Slice -> Bool
isSuffixOf :: !String !Slice -> Bool
//TODO isInfixOf :: !Slice !Slice -> Bool

////////////////////////////////////////////////////////////////////////////////
/// # Splitable
////////////////////////////////////////////////////////////////////////////////

// split :: !Char !Slice -> [Slice]
// splitWith :: (Char -> Bool) !Slice -> [Slice]

// lines :: !Slice -> [Slice]
// words :: !Slice -> [Slice]

////////////////////////////////////////////////////////////////////////////////
/// # Foldable
////////////////////////////////////////////////////////////////////////////////

// foldl :: (a Char -> a) a Slice -> a
// foldl` :: (a Char -> a) a !Slice -> a
// foldl1 :: (Char Char -> Char) Slice -> a
// foldl1` :: (Char Char -> Char) !Slice -> a

// foldr :: (a Char -> a) a Slice -> a
// foldr` :: (a Char -> a) a !Slice -> a
// foldr1 :: (Char Char -> Char) Slice -> a
// foldr1` :: (Char Char -> Char) !Slice -> a

isEmpty :: !Slice -> Bool
length :: !Slice -> Int

// any :: !(Char -> Bool) !Slice -> Bool
// all :: !(Char -> Bool) !Slice -> Bool
// maximum :: !Slice -> Bool
// minimum :: !Slice -> Bool

////////////////////////////////////////////////////////////////////////////////
/// # Indexable
////////////////////////////////////////////////////////////////////////////////

findIndex :: !(Char -> Bool) !Slice -> Maybe Int
findIndexOrEnd :: !(Char -> Bool) !Slice -> Int

