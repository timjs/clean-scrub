implementation module Data.Slice

import Base

import qualified Data.String as String

////////////////////////////////////////////////////////////////////////////////
/// # Data Type
////////////////////////////////////////////////////////////////////////////////

:: Slice =
    { string :: !String
    , position :: !Int
    , length :: !Int
    }

// instance == Slice where
//     (==) left right
//         | left.length <> right.length = False
//         = check (left.length - 1)
//         where
//             check 0 = left.string.[left.position] == right.string.[right.position]
//             check i = left.string.[left.position + i] == right.string.[right.position + i] && check (i - 1)

/// Instance of Eq
instance == Slice where
    (==) left right
        | left.length <> right.length = False
        = check left.position right.position
        where
            lmax = left.position + left.length
            // rmax = right.position + right.length
            check l r
                // | l < lmax && r < rmax = left.string.[l] == right.string.[r] && check (l + 1) (r + 1)
                | l < lmax = left.string.[l] == right.string.[r] && check (l + 1) (r + 1)
                = True

/// Instance of Ord
instance < Slice where
    (<) left right
        | left.length > right.length = False
        = check left.position right.position
        where
            lmax = left.position + left.length
            check l r
                # lchar = left.string.[l]
                # rchar = right.string.[r]
                | l < lmax
                    | lchar <  rchar = True
                    | lchar == rchar = check (l + 1) (r + 1)
                    = False
                = left.length < right.length

/// Instance of Sliceable
instance % Slice where
    (%) {string,position,length} (upper,lower) =
        { string = string // Retains the string
        , position = position + min upper length
        , length = max (lower - upper + 1) 0
        }

///TODO Instance of Print

///TODO Instance of Scan

////////////////////////////////////////////////////////////////////////////////
/// # Primitives
////////////////////////////////////////////////////////////////////////////////

empty :: Slice
empty = wrap 'String'.empty

singleton :: !Char -> Slice
singleton char = wrap ('String'.singleton char)

slice :: !Int !Int !String -> Slice
slice upper lower string = wrap string % (upper, lower)

wrap :: !String -> Slice
wrap string =
    { string = string // Retains the string
    , position = 0
    , length = size string
    }

unwrap :: !Slice -> String
unwrap {string,position,length} = string % (position, position + length - 1) // Releases the string

////////////////////////////////////////////////////////////////////////////////
/// # Basics
////////////////////////////////////////////////////////////////////////////////

/// ## Safe basics

head :: !Slice -> Maybe Char
head slice
    | slice.length <= 0 = Nothing
    = Just $ unsafeHead slice

tail :: !Slice -> Maybe Slice
tail slice
    | slice.length <= 0 = Nothing
    = Just $ unsafeTail slice

init :: !Slice -> Maybe Slice
init slice
    | slice.length <= 0 = Nothing
    = Just $ unsafeInit slice

last :: !Slice -> Maybe Char
last slice
    | slice.length <= 0 = Nothing
    = Just $ unsafeLast slice

uncons :: !Slice -> Maybe (Char,Slice)
uncons slice
    | slice.length <= 0 = Nothing
    = Just (unsafeHead slice, unsafeTail slice)

unsnoc :: !Slice -> Maybe (Slice,Char)
unsnoc slice
    | slice.length <= 0 = Nothing
    = Just (unsafeInit slice, unsafeLast slice)

/// ### Unsafe basics

unsafeHead :: !Slice -> Char
unsafeHead {string,position,length} = string.[position]

unsafeTail :: !Slice -> Slice
unsafeTail {string,position,length} =
    { string = string
    , position = position + 1
    , length = length - 1
    }

unsafeInit :: !Slice -> Slice
unsafeInit {string,position,length} =
    { string = string
    , position = position
    , length = length - 1
    }

unsafeLast :: !Slice -> Char
unsafeLast {string,position,length} = string.[position + length - 1]

unsafeUncons :: !Slice -> (Char,Slice)
unsafeUncons slice
    = (unsafeHead slice, unsafeTail slice)

unsafeUnsnoc :: !Slice -> (Slice,Char)
unsafeUnsnoc slice
    = (unsafeInit slice, unsafeLast slice)

/// ## Length

isEmpty :: !Slice -> Bool
isEmpty {string,position,length} = length == 0

length :: !Slice -> Int
length {string,position,length} = length

////////////////////////////////////////////////////////////////////////////////
/// # Slicable
////////////////////////////////////////////////////////////////////////////////

/// O(1) `take n`, applied to a `Slice` `s` returns the prefix of `s`
/// of `length n`, or `s` itself if `n > length s`.
take :: !Int !Slice -> Slice
take n slice
    | n <= 0 = empty
    | n >= slice.length = slice
    = slice % (0, n - 1)

/// O(1) `drop n s` returns the suffix of `s` after the first `n` elements,
/// or `""` if `n > size s`.
drop :: !Int !Slice -> Slice
drop n slice
    | n <= 0 = slice
    | n >= slice.length = empty
    = slice % (n, slice.length - 1)

/// O(1) `splitAt n s` is equivalent to `(take n s, drop n s)`.
splitAt :: !Int !Slice -> (Slice, Slice)
splitAt n slice
    | n <= 0 = (empty, slice)
    | n >= slice.length = (slice, empty)
    = (slice % (0, n - 1), slice % (n, slice.length - 1))

takeTill :: !(Char -> Bool) !Slice -> Slice
takeTill predicate slice
    = take (findIndexOrEnd predicate slice) slice

dropTill :: !(Char -> Bool) !Slice -> Slice
dropTill predicate slice
    = drop (findIndexOrEnd predicate slice) slice

/// `break p s` is equivalent to `(takeTill p s, dropTill p s)`.
break :: !(Char -> Bool) !Slice -> (Slice, Slice)
break predicate slice
    = splitAt (findIndexOrEnd predicate slice) slice

/// `takeWhile`, applied to a predicate `p` and a `Slice` `s`, returns the longest prefix (possibly empty) of `s` of elements that satisfy `p`.
takeWhile :: !(Char -> Bool) !Slice -> Slice
takeWhile predicate slice
    = takeTill (not o predicate) slice

/// `dropWhile p s` returns the suffix remaining after `takeWhile p s`.
dropWhile :: !(Char -> Bool) !Slice -> Slice
dropWhile predicate slice
    = dropTill (not o predicate) slice

/// `span p s` breaks the `Slice` into two segments.
/// It is equivalent to `(takeWhile p s, dropWhile p s)`
/// and to `break (not o p)`.
span :: !(Char -> Bool) !Slice -> (Slice, Slice)
span predicate slice
    = break (not o predicate) slice

/// ## Predicates

isPrefixOf :: !String !Slice -> Bool
isPrefixOf string haystack
    # needle = wrap string
    | needle.length == 0 = True
    | needle.length > haystack.length = False
    = needle == haystack % (0, needle.length - 1)

isSuffixOf :: !String !Slice -> Bool
isSuffixOf string haystack
    # needle = wrap string
    | needle.length == 0 = True
    | needle.length > haystack.length = False
    = needle == haystack % (haystack.length - needle.length, haystack.length - 1)

//TODO isInfixOf :: !Slice !Slice -> Bool

////////////////////////////////////////////////////////////////////////////////
/// # Indexable
////////////////////////////////////////////////////////////////////////////////

/// The `findIndex` function takes a predicate and a `Slice` and
/// returns the index of the first element in the `Slice`
/// satisfying the predicate.
findIndex :: !(Char -> Bool) !Slice -> Maybe Int
findIndex predicate {string,position,length}
    = go 0
    where
        go n
            | n >= length = Nothing
            | predicate string.[position + n] = Just n
            = go (n + 1)

/// `findIndexOrEnd` is a variant of `findIndex`, that returns the size
/// of the slice if no element is found, rather than `Nothing`.
findIndexOrEnd :: !(Char -> Bool) !Slice -> Int
findIndexOrEnd predicate {string,position,length}
    = go 0
    where
        go n
            | n >= length = length
            | predicate string.[position + n] = n
            = go (n + 1)

////////////////////////////////////////////////////////////////////////////////
/// # Tests
////////////////////////////////////////////////////////////////////////////////

/*
import qualified Data.List as List

test_quick :: [Bool]
test_quick =
    [ empty == wrap ""
    , singleton 'h' == wrap "h"
    // , pack ['a', 'b', 'c'] == "abc"
    // , unpack "abc" == ['a', 'b', 'c']
    , uncons input == Just ('H', wrap "ello world")
    , isEmpty input == False
    , isEmpty empty == True
    , length input == 11
    , take 3 input == wrap "Hel"
    , drop 3 input == wrap "lo world"
    , splitAt 4 input == (take 4 input, drop 4 input)
    , takeTill p0 input == wrap "Hello"
    , dropTill p0 input == wrap " world"
    , break p1 input == (takeTill p1 input, dropTill p1 input)
    , takeWhile p2 input == wrap "Hello"
    , dropWhile p2 input == wrap " world"
    , span p1 input == break (not o p1) input
    , isPrefixOf "Hello" input
    , isSuffixOf "world" input
    ]
    where
        input = wrap "Hello world"
        p0 c = c == ' '
        p1 c = c == 'l'
        p2 c = isAlphanum c

test_cmp :: [(Bool,Bool)]
test_cmp =
    // 'List'.zipWith (==) ('List'.map wrap strings) ('List'.repeat $ wrap "abc") == 'List'.zipWith (==) strings ('List'.repeat "abc") ++
    zip2 ('List'.zipWith (<) strings bases) ('List'.zipWith (<) ('List'.map wrap strings) ('List'.map wrap bases))
    where
        strings = ["abc", "ab", "aa", "aab", "abcd", "bc"]
        bases = 'List'.repeat "abc"

Start = test_cmp
*/

