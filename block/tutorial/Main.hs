module Main (main) where

--------------------------------------------------------------------------------

-- The block type we'll demonstrate with
import Block (Text1)

-- State
import Block (State, get, modify, runState, evalState, stateless)

-- Miscellaneous other types
import Block (End (..), Shortfall (..), Take (..), Pivot (..), Span (..))

-- Qualified import of Block because some function names conflict with Prelude
import qualified Block

--------------------------------------------------------------------------------

import Control.Monad (unless)
import Data.Char (isUpper, isLetter, isSpace, isDigit, isAscii)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor (($>))
import GHC.Stack (HasCallStack)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------

hi :: Text1
hi = "Hello World"

lyric :: Text1
lyric = "on the wall, 98 bottles of beer"

--------------------------------------------------------------------------------

main :: HasCallStack => IO ()
main =
  do
    {- The 'take' function splits a block into two parts, with a
       particular specified length for the first part. -}

    (lyric & Block.take Front 5) ~> TakePart "on th" "e wall, 98 bottles of beer"
    (lyric & Block.take Back  5) ~> TakePart " beer" "on the wall, 98 bottles of"


    {- If the requested length is exactly the content length, the result is
       'TakeAll'. If the requested length is longer than the content length,
       the result is 'TakeInsufficient', which includes a 'Shortfall' number:
       the block's length subtracted from the requested length. -}

    (hi & Block.take Front  9) ~> TakePart "Hello Wor" "ld"
    (hi & Block.take Front 10) ~> TakePart "Hello Worl" "d"
    (hi & Block.take Front 11) ~> TakeAll
    (hi & Block.take Front 12) ~> TakeInsufficient (Shortfall 1)
    (hi & Block.take Front 13) ~> TakeInsufficient (Shortfall 2)


    {- 'findPredicate' splits a block into three parts:

        1. The content preceding the first item that matches the predicate
        2. The first item that matches the predicate
        3. The remainder

       If no item matches the predicate, the result is 'Nothing'. -}

    (hi & Block.findPredicate Front isUpper) ~> Just (Pivot Nothing 'H' (Just "ello World"))
    (hi & Block.findPredicate Front isSpace) ~> Just (Pivot (Just "Hello") ' ' (Just "World"))
    (hi & Block.findPredicate Back  isUpper) ~> Just (Pivot (Just "orld") 'W' (Just "Hello "))
    (hi & Block.findPredicate Front isDigit) ~> Nothing


    {- spanPredicate splits a block into two parts:

        1. The longest prefix of items matching the predicate
        2. The remainder

       If all of the items match, the result is 'SpanAll'.
       If none of the items match, the result is 'SpanNone'. -}

    (hi & Block.spanPredicate Front isLetter) ~> SpanPart "Hello" " World"
    (hi & Block.spanPredicate Back  isLetter) ~> SpanPart "World" "Hello "
    (hi & Block.spanPredicate Front isAscii ) ~> SpanAll
    (hi & Block.spanPredicate Front isDigit ) ~> SpanNone


    {- Whereas 'spanPredicate' has a parameter of type (x -> Bool), the general
       'span' function's parameter is (x -> State s Bool). This allows you to
       collect information as you go. -}

    let charAsWord :: Char -> Maybe Word
        charAsWord x = readMaybe [x] :: Maybe Word

        add :: Char -> State Word Bool
        add x = traverse_ (\w -> modify (w +)) (charAsWord x) $> not (isSpace x)

    (("234 567" :: Text1) & Block.span Front add & runState 0)
        ~> (9, SpanPart "234" " 567")


    {- Whereas 'findPredicate' has a parameter of type (x -> Bool), the
       general 'find' function's parameter is (x -> State s (Maybe a)).
       This allows a 'find' produce a final value when a match is found. -}

    (lyric & Block.find Front (pure . charAsWord) & stateless)
        ~> Just (Pivot (Just "on the wall, ") 9 (Just "8 bottles of beer"))


    {- A larger example demonstrating both state collection and a final
       result: -}

    let calculate x =
            traverse_ (\w -> modify (w :)) (charAsWord x)
            *> case x of
                '+' -> (Just . sum)     <$> get
                '*' -> (Just . product) <$> get
                _   -> pure Nothing

    (("234+ 567*" :: Text1) & Block.find Front calculate & evalState [])
        ~> Just (Pivot (Just "234") 9 (Just " 567*"))

    (("234* 567*" :: Text1) & Block.find Front calculate & evalState [])
        ~> Just (Pivot (Just "234") 24 (Just " 567*"))


--------------------------------------------------------------------------------

{-| Assert that the computed value on the left is equal to the expected
    result on the right. Otherwise, the resulting error includes a stack
    trace with line numbers to help track down which assertion failed. -}
(~>) :: HasCallStack => (Eq a, Show a) => a -> a -> IO ()
(~>) actual expected = unless (actual == expected) (pure $! error (show actual))
