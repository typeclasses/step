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
    take Front 1 (_ :| []) = TakeAll
    take Front 1 (x :| y : z) = TakePart (x :| []) (y :| z)
    take Front n (_ :| []) = TakeInsufficient (Shortfall (n - 1))
    take Front n (x :| y : z) = take Front (n - 1) (y :| z) & \case
        TakeAll -> TakeAll
        TakePart a b -> TakePart (x :| NonEmpty.toList a) b
        t@TakeInsufficient{} -> t
    take Back n xs = take Front n (NonEmpty.reverse xs) & \case
        TakeAll -> TakeAll
        TakePart a b -> TakePart (NonEmpty.reverse a) (NonEmpty.reverse b)
        t@TakeInsufficient{} -> t
