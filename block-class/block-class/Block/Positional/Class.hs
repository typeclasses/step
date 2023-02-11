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

    take :: End -> Positive -> xs -> Take xs

instance Positional (NonEmpty xs) where

    length :: NonEmpty x -> Positive
    length = Positive.length

    take :: End -> Positive -> NonEmpty x -> Take (NonEmpty x)
    take Front 1 xs@(_ :| []) = Take xs Nothing
    take Front n (_ :| []) = TakeInsufficient (Shortfall (n - 1))
    take Front n (x :| y : z) = take Front (n - 1) (y :| z) & \case
        Take a b -> Take (x :| NonEmpty.toList a) b
        s -> s
    take Back n xs = take Front n (NonEmpty.reverse xs) & \case
        Take a b -> Take (NonEmpty.reverse a) (NonEmpty.reverse <$> b)
        s -> s
