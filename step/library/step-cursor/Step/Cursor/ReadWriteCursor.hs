{-# language FlexibleInstances, FunctionalDependencies, GADTs #-}

module Step.Cursor.ReadWriteCursor
  (
    ReadWriteCursor (..),
    rebaseCursor,
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
    , visibleStateLens :: Lens' s' s
    , input :: Stream r s' m xs x
    , commit :: Positive Natural -> RST r s' m AdvanceResult
    }

rebaseCursor :: Monad m1 => (forall a. m1 a -> m2 a) -> ReadWriteCursor xs x r s m1 -> ReadWriteCursor xs x r s m2
rebaseCursor o ReadWriteCursor{ init, commit, input, visibleStateLens } =
  ReadWriteCursor
    { init = hoist o init
    , visibleStateLens
    , commit = hoist o . commit
    , input = over streamRST (hoist o) input
    }

instance Contravariant (ReadWriteCursor xs x r s m) (ReadWriteCursor xs x r' s m) r r' where
    contramap f ReadWriteCursor{ init, commit, input, visibleStateLens } =
      ReadWriteCursor
        { init = contramap f init
        , visibleStateLens
        , input = contramap f input
        , commit = contramap f . commit
        }
