{-# language GADTs #-}

module Step.Cursor.Type
  (
    Cursor (..),
    rebase,
    recurse,
  )
  where

import Step.Internal.Prelude

import Step.Cursor.ChunkStream (Stream)
import qualified Step.Cursor.ChunkStream as Stream
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

recurse :: (forall a. Iso' (cursor1 a) (cursor2 a)) -> Cursor xs x base cursor1 -> Cursor xs x base cursor2
recurse i Cursor{ run, commit, input } =
    Cursor{ commit = \n -> view i $ commit n, input = Stream.rebase (view i) input, run = run . review i }
