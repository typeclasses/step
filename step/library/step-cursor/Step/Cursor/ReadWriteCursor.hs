{-# language FlexibleInstances, FunctionalDependencies, GADTs #-}

module Step.Cursor.ReadWriteCursor
  (
    ReadWriteCursor (..),
    rebaseCursor,
    CursorState, ephemeralStateLens, committedStateLens,
  )
  where

import Step.Internal.Prelude

import Step.Cursor.Stream (Stream, streamRST)

import Step.Cursor.AdvanceResult (AdvanceResult)

import Step.RST

import Optics

data ReadWriteCursor xs x r s m =
  forall s'. ReadWriteCursor
    { init :: RST r s m s'
    , input :: Stream r (CursorState s' s) m xs x
    , commit :: Positive Natural -> RST r (CursorState s' s) m AdvanceResult
    }

data CursorState ephemeral committed =
    CursorState{ ephemeralState :: ephemeral, committedState :: committed }

ephemeralStateLens = lens ephemeralState \x y -> x{ ephemeralState = y }

committedStateLens = lens committedState \x y -> x{ committedState = y }

rebaseCursor :: Monad m1 => (forall a. m1 a -> m2 a) -> ReadWriteCursor xs x r s m1 -> ReadWriteCursor xs x r s m2
rebaseCursor o ReadWriteCursor{ init, commit, input } =
  ReadWriteCursor
    { init = hoist o init
    , commit = hoist o . commit
    , input = over streamRST (hoist o) input
    }

instance Contravariant (ReadWriteCursor xs x r s m) (ReadWriteCursor xs x r' s m) r r' where
    contramap f ReadWriteCursor{ init, commit, input } =
      ReadWriteCursor
        { init = contramap f init
        , input = contramap f input
        , commit = contramap f . commit
        }
