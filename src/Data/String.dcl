definition module Data.String

from StdString import class %(..), instance % {#Char}
from StdString import class +++(..), instance +++ {#Char}

from Data.Maybe import :: Maybe

////////////////////////////////////////////////////////////////////////////////
/// # Instances
////////////////////////////////////////////////////////////////////////////////

// instance Foldable {}
foldr :: (Char a -> a) a String -> a
foldr` :: (Char a -> a) a String -> a
foldl :: (a Char -> a) a String -> a
foldl` :: (a Char -> a) a String -> a
foldr1 :: (Char Char -> Char) String -> Char
foldl1 :: (Char Char -> Char) String -> Char

////////////////////////////////////////////////////////////////////////////////
/// # Primitives
////////////////////////////////////////////////////////////////////////////////

empty :: String
singleton :: !Char -> String
pack :: ![Char] -> String
unpack :: !String -> [Char]

////////////////////////////////////////////////////////////////////////////////
/// # Basics
////////////////////////////////////////////////////////////////////////////////

uncons :: !String -> Maybe (Char,String)
unsafeUncons :: !String -> (Char,String)
null :: !String -> Bool
length :: !String -> Int

////////////////////////////////////////////////////////////////////////////////
/// # Slicable
////////////////////////////////////////////////////////////////////////////////

take :: !Int !String -> String
drop :: !Int !String -> String	
splitAt :: !Int !String -> (String, String)

takeTill :: !(Char -> Bool) !String -> String
dropTill :: !(Char -> Bool) !String -> String
break :: !(Char -> Bool) !String -> (String, String)

takeWhile :: !(Char -> Bool) !String -> String
dropWhile :: !(Char -> Bool) !String -> String
span :: !(Char -> Bool) !String -> (String, String)

isPrefixOf :: !String !String -> Bool
isSuffixOf :: !String !String -> Bool

////////////////////////////////////////////////////////////////////////////////
/// # Indexable
////////////////////////////////////////////////////////////////////////////////

findIndex :: !(Char -> Bool) !String -> Maybe Int
findIndexOrEnd :: !(Char -> Bool) !String -> Int

