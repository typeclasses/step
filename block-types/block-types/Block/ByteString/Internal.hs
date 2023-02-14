module Block.Class.ByteString.Internal
  (
    {- * Type -} ByteString1 (..),
  )
  where

import Essentials
import Block.Class

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Maybe (fromJust, fromMaybe)
import Data.Word (Word8)

import qualified Data.Foldable as Foldable
import qualified Data.ByteString as ByteString
import qualified Block.Class as Block
import Prelude (error)

newtype ByteString1 = ByteString1 ByteString
    deriving stock (Eq, Ord, Show)

instance Semigroup ByteString1 where
    ByteString1 a <> ByteString1 b = ByteString1 (a <> b)

instance Trivializable ByteString1 where
    refine x = if ByteString.null x then Nothing else Just (ByteString1 x)
    assume = refine >>> fromMaybe (error "assume @ByteString1: empty")
    generalize (ByteString1 x) = x

type instance Block.Item ByteString1 = Word8

type instance Nullable ByteString1 = ByteString

instance Block ByteString1 where
    singleton = ByteString.singleton >>> ByteString1
    concat = Foldable.toList >>> fmap generalize >>> ByteString.concat >>> ByteString1
    pop Left  xs = let (x, xs') = fromJust (ByteString.uncons (generalize xs)) in Pop x (refine xs')
    pop Right xs = let (xs', x) = fromJust (ByteString.unsnoc (generalize xs)) in Pop x (refine xs')
    push Left  x xs = ByteString1 (ByteString.cons x (generalize xs))
    push Right x xs = ByteString1 (ByteString.snoc (generalize xs) x)

