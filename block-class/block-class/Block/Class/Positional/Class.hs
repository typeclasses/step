module Block.Class.Positional.Class
  (
    Positional (..),
  )
  where

import Essentials

import Block.Class.End (End (..))
import Block.Class.Positional.Types (Take (..))
import Block.Class.Shortfall (Shortfall (..))
import Data.List.NonEmpty (NonEmpty (..))
import Integer (Positive)
import Prelude ((-))

import qualified Integer.Positive as Positive
import qualified Data.List.NonEmpty as NonEmpty

class Positional x xs | xs -> x where

    {-| The number of items in the block -}
    length :: xs -> Positive

    {-| Get the item at a particular position

    Returns 'Nothing' if the position is greater than 'length'.

    The first item's position is 1. (Please take note, because
    this is unconventional.) -}
    at :: End -> Positive -> xs -> Maybe x

    {-| Separate a block into two parts by specifying the length
        of the first part

    When possible, the result is @('TakePart' taken remainder)@,
    where @taken@ is of the requested length and @remainder@ is
    everything else. If there is no remainder (when the requested
    number of items is exactly the block length), the result is
    'TakeAll'. If there are not enough items to take the requested
    number, the result is @('TakeInsufficient' s)@ where @s@ is a
    'Shortfall' indicating the difference between the block size
    and the requested number. -}
    take ::
        End -- ^ Which end to take from: 'Front' or 'Back'
        -> Positive -- ^ How many items to take
        -> xs -- ^ A block
        -> Take xs

instance Positional x (NonEmpty x) where

    length :: NonEmpty x -> Positive
    length = Positive.length

    at :: End -> Positive -> NonEmpty x -> Maybe x
    at Front 1 = \(x :| _) -> Just x
    at Front n = \(_ :| xs) -> go (n - 1) xs
      where
        go _ [] = Nothing
        go 1 (x : _) = Just x
        go i (_ : xs) = go i xs
    at Back n = NonEmpty.reverse >>> at Front n

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
