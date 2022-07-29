module Step.Cursor.Type
  (
    Cursor (..),
    rebaseCursor,
  )
  where

import Step.Internal.Prelude

import Step.Cursor.Stream (Stream, rebaseStream)

import Step.Cursor.AdvanceResult (AdvanceResult)

import Step.RST

data Cursor xs x r s s' m =
  Cursor
    { init :: RST r s' m s
    , input :: Stream (RST r s m) xs x
    , commit :: Positive Natural -> RST r s m AdvanceResult
    , extract :: RST r s m s'
    }

rebaseCursor :: (forall a. m1 a -> m2 a) -> Cursor xs x r s s' m1 -> Cursor xs x r s s' m2
rebaseCursor o Cursor{ init, commit, input, extract } =
  Cursor
    { init = mapRST o init
    , commit = mapRST o . commit
    , input = rebaseStream (mapRST o) input
    , extract = mapRST o extract
    }
