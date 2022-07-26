{-# language GADTs #-}

module Step.Cursor.Type
  (
    Cursor (..),
    rebase,
  )
  where

import Step.Internal.Prelude

import Step.Cursor.ChunkStream (Stream)
import Step.Cursor.AdvanceResult (AdvanceResult)

data Cursor xs x m =
  forall m'. Monad m' => Cursor
    { run :: forall a. m' a -> m a
    , commit :: Positive Natural -> m' AdvanceResult
    , input :: Stream m' xs x
    }

rebase :: (forall a. m1 a -> m2 a) -> Cursor xs x m1 -> Cursor xs x m2
rebase f Cursor{ run, commit, input } =
    Cursor{ commit = commit, input = input, run = f . run }
