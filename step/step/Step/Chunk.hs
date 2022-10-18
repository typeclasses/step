module Step.Chunk where

import Data.Eq (Eq)
import Data.Function ((.))
import Data.Functor (Functor)
import Data.Functor.Contravariant (Predicate (..), Equivalence (..))
import Data.Kind (Type)
import Data.Maybe (Maybe (..))
import Data.Ord (Ord)
import NatOptics.Positive.Unsafe (Positive)
import Numeric.Natural (Natural)
import Text.Show (Show)
import Data.Ord (Ord (compare), Ordering (..))
import Data.Bool (Bool (..))
import Prelude (error)
import Data.Foldable (Foldable)

type family One (c :: Type) :: Type

class Chunk c
  where
    leftView :: c -> Pop c
    span :: Predicate (One c) -> c -> Span c
    split :: Positive Natural -> c -> Split c
    take :: Positive Natural -> c -> Take c
    drop :: Positive Natural -> c -> Drop c
    while :: Predicate (One c) -> c -> While c
    length :: c -> Positive Natural
    concat :: [c] -> c

data StripEitherPrefix c =
    StripEitherPrefixAll
  | StripEitherPrefixFail
  | IsPrefixOf   { afterPrefix :: c }
  | IsPrefixedBy { afterPrefix :: c }

data Pop c =
  Pop
    { popItem :: One c
    , popRemainder :: Maybe c
    }

data Span c =
    SpanAll
  | SpanNone
  | SpanPart
      { spannedPart :: c
      , spanRemainder :: c
      }
  deriving stock (Eq, Ord, Show)

data Split c =
    SplitInsufficient
  | Split c c
  deriving stock (Eq, Ord, Show)

data Drop c =
    DropAll
  | DropInsufficient
      { dropShortfall :: Positive Natural
      }
  | DropPart
      { dropRemainder :: c
      }
  deriving stock (Eq, Ord, Show)

data Take c =
    TakeAll
  | TakeInsufficient
      { takeShortfall :: Positive Natural
      }
  | TakePart
      { takePart :: c
      }

data While c =
    WhileNone
  | WhilePrefix c
  | WhileAll

head :: Chunk c => c -> One c
head = popItem . leftView

-- | An equivalence on characters, expressed as an equivalence on chunks.
--
-- It must be the case that chunks /a/ and /b/ are equivalent iff the length
-- of /a/ and /b/ is /l/ and /a[i] ~ b[i]/ for all /i = [1 .. l]/ according
-- to the character equivalence.
--
newtype ChunkCharacterEquivalence c =
    ChunkCharacterEquivalence{ chunksEquivalent :: c -> c -> Bool }

stripEitherPrefix :: Chunk c => ChunkCharacterEquivalence c -> c -> c -> StripEitherPrefix c
stripEitherPrefix eq a b = case compare (length a) (length b) of
    EQ -> if chunksEquivalent eq a b
            then StripEitherPrefixAll
            else StripEitherPrefixFail
    LT -> case split (length a) b of
        Split a' b' -> if chunksEquivalent eq a a'
            then IsPrefixOf{ afterPrefix = b' }
            else StripEitherPrefixFail
        SplitInsufficient{} -> error "stripEitherPrefix"
    GT -> case split (length b) a of
        Split b' a' -> if chunksEquivalent eq b b'
            then IsPrefixedBy{ afterPrefix = a' }
            else StripEitherPrefixFail
        SplitInsufficient{} -> error "stripEitherPrefix"
