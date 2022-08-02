{-# language Unsafe #-}

module Step.Nontrivial.ListLike.Construction where

import Step.Internal.Prelude

import Step.Nontrivial.Unsafe

import Step.Nontrivial.Operations

import qualified ListLike

import qualified Positive
import qualified Positive.Unsafe as Positive
import qualified Positive.Math as Positive
import qualified Signed

untrivializeOperation :: ListLike xs x => UntrivializeOperation xs x
untrivializeOperation = UntrivializeOperation \x -> do
    l <- (preview Positive.natPrism . fromIntegral . ListLike.length) x
    return
      NontrivialUnsafe
        { generalize = x
        , length = l
        , head = ListLike.head x
        , tail = untrivialize untrivializeOperation (ListLike.tail x)
        }

nontrivialUnsafe :: ListLike xs x => xs -> Nontrivial xs x
nontrivialUnsafe x =
    NontrivialUnsafe
      { generalize = x
      , length = (Positive.PositiveUnsafe . fromIntegral . ListLike.length) x
      , head = ListLike.head x
      , tail = untrivialize untrivializeOperation (ListLike.tail x)
      }
