module Step.Buffer.Buffer where

import Step.Chunk
import Step.Interface
import qualified Step.Interface.Core as I

-- The basics
import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..), isNothing)
import Data.Functor (Functor (..), (<&>), ($>), (<$>))
import Data.Function (($))
import Control.Monad (Monad (..))
import Control.Applicative (Applicative (..))
import Data.Foldable (toList)

-- Containers
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- Optics
import Optics (Lens', use, assign, modifying)

-- Math
import Numeric.Natural (Natural)
import NatOptics.Positive.Unsafe (Positive)

-- Transformers
import Control.Monad.State.Strict (MonadState)

-- Streaming
import SupplyChain (Vendor (..), Job, Referral (..), (>->), order)
import SupplyChain.Interface.TerminableStream (IsTerminableStream, TerminableStream)
import qualified SupplyChain
import qualified SupplyChain.Interface.TerminableStream as Stream

newtype Buffer c = Buffer{ bufferSeq :: Seq c }

pattern One :: a -> Buffer a
pattern One x = Buffer (x Seq.:<| Seq.Empty)

pattern (:<) :: c -> Buffer c -> Buffer c
pattern x :< xs <- Buffer (x Seq.:<| (Buffer -> xs))
  where
    x :< Buffer xs = Buffer (x Seq.:<| xs)

pattern (:>) :: Buffer c -> c -> Buffer c
pattern xs :> x <- Buffer ((Buffer -> xs) Seq.:|> x)
  where
    Buffer xs :> x = Buffer (xs Seq.:|> x)

pattern Empty :: Buffer a
pattern Empty = Buffer Seq.Empty

pattern List :: [a] -> Buffer a
pattern List xs <- Buffer (toList -> xs)
  where
    List xs = Buffer (Seq.fromList xs)

{-# complete Empty, (:>) #-}
{-# complete Empty, (:<) #-}
{-# complete List #-}
