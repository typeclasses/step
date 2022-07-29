module Step.Cursor.Type
  (
    Cursor (..),
    rebaseCursor,
    expandStateCursor,
  )
  where

import Step.Internal.Prelude

import Step.Cursor.Stream (Stream, streamRST)

import Step.Cursor.AdvanceResult (AdvanceResult)

import Step.RST

import Optics

data Cursor xs x r s s' m =
  Cursor
    { init :: s' -> s
    , extract :: s -> s'
    , input :: Stream r s m xs x
    , commit :: Positive Natural -> RST r s m AdvanceResult
    }

rebaseCursor :: Monad m1 => (forall a. m1 a -> m2 a) -> Cursor xs x r s s' m1 -> Cursor xs x r s s' m2
rebaseCursor o Cursor{ init, commit, input, extract } =
  Cursor
    { init = init
    , extract = extract
    , commit = hoist o . commit
    , input = over streamRST (hoist o) input
    }

expandStateCursor :: forall extra xs x r a b m. Monad m =>
    Cursor xs x r a b m -> Cursor xs x r (extra, a) (extra, b) m
expandStateCursor Cursor{ init, commit, input, extract } =
  Cursor
    { init = over _2 init
    , extract = over _2 extract
    , input = over streamRST (zoom _2) input
    , commit = zoom _2 . commit
    }
