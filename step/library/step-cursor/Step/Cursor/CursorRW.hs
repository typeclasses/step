{-# language FlexibleInstances, FunctionalDependencies, GADTs #-}

module Step.Cursor.CursorRW
  (
    CursorRW (..),
    rebaseCursor,
  )
  where

import Step.Internal.Prelude

import Step.Cursor.Stream (Stream, streamRST)
import qualified Step.Cursor.Stream as Stream

import Step.Cursor.AdvanceResult (AdvanceResult)

import Step.RST

data CursorRW xs x r s m =
  forall s'. CursorRW
    { init :: RST r s m s'
    , visibleStateLens :: Lens' s' s
    , input :: Stream r s' m xs x
    , commit :: Positive Natural -> RST r s' m AdvanceResult
    }

rebaseCursor :: Monad m1 => (forall a. m1 a -> m2 a) -> CursorRW xs x r s m1 -> CursorRW xs x r s m2
rebaseCursor o CursorRW{ init, commit, input, visibleStateLens } =
  CursorRW
    { init = hoist o init
    , visibleStateLens
    , commit = hoist o . commit
    , input = over streamRST (hoist o) input
    }

instance Contravariant (CursorRW xs x r s m) (CursorRW xs x r' s m) r r' where
    contramap f CursorRW{ init, commit, input, visibleStateLens } =
      CursorRW
        { init = contramap f init
        , visibleStateLens
        , input = contramap f input
        , commit = contramap f . commit
        }
