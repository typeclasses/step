{-# language Safe #-}

module Integer.Integer
  (
    {- * Type -} Integer,
    {- * Arithmetic -} {- $arithmetic -}
    {- * Conversion -}
    {- ** Positive -} toPositive, fromPositive,
    {- ** Natural -} toNatural, fromNatural,
    {- ** Signed -} toSigned, fromSigned,
  )
  where

import Data.Maybe (Maybe (..))
import Integer.Positive (Positive)
import Integer.Signed (Signed (..))
import Numeric.Natural (Natural)
import Prelude (Integer)

import qualified Integer.Natural as Natural
import qualified Integer.Positive as Positive
import qualified Integer.Signed as Signed

{- $arithmetic

This module offers no arithmetic operations because
"Prelude.Num" class already suits the integer well enough.

-}

toPositive :: Integer -> Maybe Positive
toPositive = Positive.fromInteger

fromPositive :: Positive -> Integer
fromPositive = Positive.toInteger

toNatural :: Integer -> Maybe Natural
toNatural = Natural.fromInteger

fromNatural :: Natural -> Integer
fromNatural = Natural.toInteger

toSigned :: Integer -> Signed
toSigned = Signed.fromInteger

fromSigned :: Signed -> Integer
fromSigned = Signed.toInteger
