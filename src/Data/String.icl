implementation module Data.String

import Prelude

import qualified Data.List as List

from StdString import instance % {#Char}, instance +++ {#Char}

////////////////////////////////////////////////////////////////////////////////
/// # Instances
////////////////////////////////////////////////////////////////////////////////

// instance Foldable {} where

foldr :: (Char a -> a) a String -> a
foldr f x y = 'List'.foldr f x (fromString y) //TODO optimize

foldl :: (a Char -> a) a String -> a
foldl f x y = 'List'.foldl f x (fromString y) //TODO optimize

foldr1 :: (Char Char -> Char) String -> Char
foldr1 f x = 'List'.foldr1 f (fromString x)

foldl1 :: (Char Char -> Char) String -> Char
foldl1 f x = 'List'.foldl1 f (fromString x)

foldr` :: (Char a -> a) a String -> a
foldr` f z0 xs = foldl f` id xs z0
    where f` k x z = k (f x z)

foldl` :: (a Char -> a) a String -> a
foldl` f z0 xs = foldr f` id xs z0
    where f` x k z = k (f z x)

// fold x = foldMap id x
// foldMap f x = foldr (mappend o f) mempty x

////////////////////////////////////////////////////////////////////////////////
/// # Primitives
////////////////////////////////////////////////////////////////////////////////

empty :: String
empty = ""

singleton :: !Char -> String
singleton char = toString char

pack :: ![Char] -> String
pack chars = toString chars

unpack :: !String -> [Char]
unpack string = fromString string

////////////////////////////////////////////////////////////////////////////////
/// # Basics
////////////////////////////////////////////////////////////////////////////////

cons :: !Char !String -> String
cons char string
    = singleton char +++ string

//TODO snoc :: !String !Char -> String

//TODO append :: !String !String -> String

//TODO head :: !String -> Char

//TODO tail :: !String -> String

//TODO init :: !String -> String

//TODO last :: !String -> String

uncons :: !String -> Maybe (Char,String)
uncons string
    #! s = size string
    | s <= 0 = Nothing
    #! head = select string 0
    #! tail = string % (1, s - 1)
    = Just (head, tail)

unsafeUncons :: !String -> (Char,String)
unsafeUncons string
    # head = select string 0
    # s = size string
    # tail = string % (1, s - 1)
    = (head, tail)

//TODO unsnoc :: !String -> Maybe (String,Char)

null :: !String -> Bool
null string = size string == 0

length :: !String -> Int
length string = size string

////////////////////////////////////////////////////////////////////////////////
/// # Sliceable
////////////////////////////////////////////////////////////////////////////////

/// O(1) `take n`, applied to a `String` `s` returns the prefix of `s`
/// of `length n`, or `s` itself if `n > length s`.
take :: !Int !String -> String
take n string
    | n <= 0 = empty
    #! s = size string
    | n >= s = string
    = string % (0, n - 1)

/// O(1) `drop n s` returns the suffix of `s` after the first `n` elements,
/// or `""` if `n > size s`.
drop :: !Int !String -> String
drop n string
    | n <= 0 = string
    #! s = size string
    | n >= s = empty
    = string % (n, s - 1)

/// O(1) `splitAt n s` is equivalent to `(take n s, drop n s)`.
splitAt :: !Int !String -> (String, String)
splitAt n string
    | n <= 0 = (empty, string)
    #! s = size string
    | n >= s = (string, empty)
    = (string % (0, n - 1), string % (n, s - 1))

takeTill :: !(Char -> Bool) !String -> String
takeTill predicate string
    = take (findIndexOrEnd predicate string) string

dropTill :: !(Char -> Bool) !String -> String
dropTill predicate string
    = drop (findIndexOrEnd predicate string) string

/// `break p s` is equivalent to `(takeTill p s, dropTill p s)`.
break :: !(Char -> Bool) !String -> (String, String)
break predicate string
    = splitAt (findIndexOrEnd predicate string) string

/// `takeWhile`, applied to a predicate `p` and a `String` `s`, returns the longest prefix (possibly empty) of `s` of elements that satisfy `p`.
takeWhile :: !(Char -> Bool) !String -> String
takeWhile predicate string
    = takeTill (not o predicate) string

/// `dropWhile p s` returns the suffix remaining after `takeWhile p s`.
dropWhile :: !(Char -> Bool) !String -> String
dropWhile predicate string
    = dropTill (not o predicate) string

/// `span p s` breaks the `String` into two segments.
/// It is equivalent to `(takeWhile p s, dropWhile p s)`
/// and to `break (not o p)`.
span :: !(Char -> Bool) !String -> (String, String)
span predicate string
    = break (not o predicate) string

/// ## Predicates

isPrefixOf :: !String !String -> Bool
isPrefixOf needle haystack
    #! needleSize = size needle
    #! haystackSize = size haystack
    = needleSize <= haystackSize &&
        needle == haystack % (0, needleSize - 1)

isSuffixOf :: !String !String -> Bool
isSuffixOf needle haystack
    #! needleSize = size needle
    #! haystackSize = size haystack
    = needleSize <= haystackSize &&
        needle == haystack % (haystackSize - needleSize, haystackSize - 1)

//TODO isInfixOf :: !String !String -> Bool

////////////////////////////////////////////////////////////////////////////////
/// # Indexable
////////////////////////////////////////////////////////////////////////////////

/// The `findIndex` function takes a predicate and a `String` and
/// returns the index of the first element in the `String`
/// satisfying the predicate.
findIndex :: !(Char -> Bool) !String -> Maybe Int
findIndex predicate string
    = go string 0
    where
        go s n
            | n >= stringSize = Nothing
            #! (char,rest) = unsafeUncons s
            | predicate char = Just n
            = go rest (n + 1)
        stringSize = size string

/// `findIndexOrEnd` is a variant of `findIndex`, that returns the size
/// of the string if no element is found, rather than `Nothing`.
findIndexOrEnd :: !(Char -> Bool) !String -> Int
findIndexOrEnd predicate string
    = go string 0
    where
        go s n
            | n >= stringSize = stringSize
            #! (char,rest) = unsafeUncons s
            | predicate char = n
            = go rest (n + 1)
        stringSize = size string

////////////////////////////////////////////////////////////////////////////////
/// # Tests
////////////////////////////////////////////////////////////////////////////////

/*
test_quick :: [Bool]
test_quick =
    [ empty == ""
    , singleton 'h' == "h"
    , pack ['a', 'b', 'c'] == "abc"
    , unpack "abc" == ['a', 'b', 'c']
    , uncons input == Just ('H', "ello world")
    , null input == False
    , null empty == True
    , size input == 11
    , take 3 input == "Hel"
    , drop 3 input == "lo world"
    , splitAt 4 input == (take 4 input, drop 4 input)
    , takeTill p0 input == "Hello"
    , dropTill p0 input == " world"
    , break p1 input == (takeTill p1 input, dropTill p1 input)
    , takeWhile p2 input == "Hello"
    , dropWhile p2 input == " world"
    , span p1 input == break (not o p1) input
    , isPrefixOf "Hello" input == True
    , isSuffixOf "world" input == True
    ]
    where
        input = "Hello world"
        p0 c = c == ' '
        p1 c = c == 'l'
        p2 c = isAlphanum c

Start = test_quick
*/
