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

data Cursor xs x base cursor =
  Cursor
    { run :: forall a. cursor a -> base a
    , commit :: Positive Natural -> cursor AdvanceResult
    , input :: Stream cursor xs x
    }

rebase :: (forall a. base1 a -> base2 a) -> Cursor xs x base1 cursor -> Cursor xs x base2 cursor
rebase f Cursor{ run, commit, input } =
    Cursor{ commit = commit, input = input, run = f . run }
