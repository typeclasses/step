module Block.Positional.Class
  (
    Positional (..),
  )
  where

import Essentials

import Block.Singleton.Class
import Block.Positional.Types

import Integer (Positive)
import Data.List.NonEmpty (NonEmpty (..))
import Block.End (End (..))
import Prelude ((-))

import qualified Integer.Positive as Positive
import qualified Data.List.NonEmpty as NonEmpty

class (Singleton xs) => Positional xs where

    length :: xs -> Positive

    span :: End -> Positive -> xs -> Span xs

instance Positional (NonEmpty xs) where

    length :: NonEmpty x -> Positive
    length = Positive.length

    span :: End -> Positive -> NonEmpty x -> Span (NonEmpty x)
    span Front 1 xs@(_ :| []) = Span xs Nothing
    span Front n (_ :| []) = SpanInsufficient (Shortfall (n - 1))
    span Front n (x :| y : z) = span Front (n - 1) (y :| z) & \case
        Span a b -> Span (x :| NonEmpty.toList a) b
        s -> s
    span Back n xs = span Front n (NonEmpty.reverse xs) & \case
        Span a b -> Span (NonEmpty.reverse a) (NonEmpty.reverse <$> b)
        s -> s
