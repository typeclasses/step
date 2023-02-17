module Block.Class.Positional.Class
  (
    Positional (..),
  )
  where

import Essentials

import Block.Class.End (End (..))
import Block.Class.Positional.Types (Take (..))
import Block.Class.Shortfall (Shortfall (..))
import Block.Class.Singleton.Class (Singleton)
import Data.List.NonEmpty (NonEmpty (..))
import Integer (Positive, Natural)
import Prelude ((-))
import Data.Int (Int)

import qualified Data.List as List
import qualified Integer.Positive as Positive
import qualified Data.List.NonEmpty as NonEmpty
import qualified Integer.Natural as Natural

class (Singleton x xs) => Positional x xs | xs -> x where

    {-| The number of items in the block -}
    length :: xs -> Positive

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

    at :: End -> Natural -> xs -> Maybe x

instance Positional x (NonEmpty x) where

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

    at :: End -> Natural -> NonEmpty x -> Maybe x
    at = \end i (NonEmpty.toList -> xs) -> do
        i' <- Natural.toInt i
        xs & ix case end of { Front -> i'; Back -> List.length xs - i' }
      where
        ix :: Int -> [x] -> Maybe x
        ix _ [] = Nothing
        ix 0 (x : _) = Just x
        ix i (_ : xs) = ix (i - 1) xs
