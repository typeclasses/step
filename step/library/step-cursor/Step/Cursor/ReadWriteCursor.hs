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

ephemeralStateLens :: Lens (CursorState ephemeral1 committed) (CursorState ephemeral2 committed) ephemeral1 ephemeral2
ephemeralStateLens = lens ephemeralState \x y -> x{ ephemeralState = y }

committedStateLens :: Lens (CursorState ephemeral committed1) (CursorState ephemeral committed2) committed1 committed2
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
