module Step.Chunk.ListLike (NonEmptyListLike) where

import Step.Chunk hiding (concat)
import Step.Chunk.ListLike.Core hiding (length, generalize)
import qualified Step.Chunk as Chunk

import Control.Applicative (pure, (<*>))
import Data.Bool (Bool (..))
import Data.Eq (Eq ((==)))
import Data.Foldable (Foldable)
import Data.Function (($), (&), (.), on)
import Data.Functor ((<&>), fmap)
import Data.Functor.Contravariant (Predicate (..), Equivalence (..))
import Data.ListLike (ListLike)
import Data.Maybe (Maybe (..), fromMaybe, maybe)
import Data.Ord (Ord (compare), Ordering (..))
import Data.Sequence (Seq (..))
import Data.String (IsString (..))
import GHC.Exts (IsList (..))
import Hedgehog (Gen)
import NatOptics.Positive.Unsafe (Positive (PositiveUnsafe))
import Numeric.Natural (Natural)
import Optics (preview, review)
import Prelude (error)
import Prelude (fromIntegral)
import Text.Show (Show (showsPrec))
import Data.Semigroup (Semigroup (..))

import qualified Data.List.NonEmpty as NE
import qualified Data.Foldable as Foldable
import qualified Data.ListLike as LL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified NatOptics.Positive as Positive
import qualified NatOptics.Positive.Math as Positive
import qualified NatOptics.Signed as Signed
