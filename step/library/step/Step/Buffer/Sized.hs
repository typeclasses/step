{-# language FlexibleInstances, FlexibleContexts, Trustworthy #-}

module Step.Buffer.Sized
  (
    SizedBuffer, isEmpty, empty, (|>),

    -- * State operations
    takeChunk, dropN,
  )
  where


import Step.Internal.Prelude hiding (fold)

import qualified Seq

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial
import qualified Step.Nontrivial.Drop as Drop

import qualified Step.Input.AdvanceResult as Advance
import Step.Input.AdvanceResult (AdvanceResult)

import qualified Positive

data SizedBuffer text char =
  SizedBuffer
    { chunks :: Seq (Nontrivial text char)
    , size :: Natural
    }

(|>) :: ListLike text char =>
    SizedBuffer text char -> Nontrivial text char -> SizedBuffer text char
SizedBuffer{ chunks, size } |> x =
  SizedBuffer
    { chunks = chunks Seq.|> x
    , size = size + Nontrivial.lengthNat x
    }

isEmpty :: SizedBuffer text char -> Bool
isEmpty = Seq.null . chunks

empty :: SizedBuffer text char
empty = SizedBuffer{ chunks = Seq.empty, size = 0 }

takeChunk :: ListLike text char => Monad m =>
    StateT (SizedBuffer text char) m (Maybe (Nontrivial text char))
takeChunk = get >>= \b -> case uncons (chunks b) of
    Nothing -> return Nothing
    Just (c, cs) ->
        put
          SizedBuffer
            { chunks = cs
            , size = size b - Nontrivial.lengthNat c
            }
        $> Just c

dropN :: (Monad m, ListLike text char) =>
    Positive Natural -> StateT (SizedBuffer text char) m AdvanceResult
dropN = fix \r n -> get >>= \case
    SizedBuffer{ chunks = Seq.Empty } ->
        return Advance.InsufficientInput{ Advance.shortfall = n }
    SizedBuffer{ chunks = (Seq.:<|) x xs, size } ->
        case Nontrivial.dropPositive n x of
            Drop.DroppedAll -> do
                put
                  SizedBuffer
                    { chunks = xs
                    , size = size - review Positive.refine n
                    }
                return Advance.Success
            Drop.DroppedPart{ Drop.remainder } -> do
                put
                  SizedBuffer
                    { chunks = (Seq.:<|) remainder xs
                    , size = size - review Positive.refine n
                    }
                return Advance.Success
            Drop.Insufficient{ Drop.shortfall } -> do
                put
                  SizedBuffer
                    { chunks = xs
                    , size = size - Nontrivial.lengthNat x
                    }
                r shortfall
