{-# language FlexibleInstances, FunctionalDependencies, GADTs #-}

module Step.Cursor.Cursor
  (
    -- * The backend interface for building cursors
    CursorR (..), CursorRW (..),
    -- ** Hoisting
    rebaseCursorR, rebaseCursorRW,

    -- * The user interface for running cursors
    RunCursorRW (..), runCursorRW,
  )
  where

import Step.Internal.Prelude

import Step.Cursor.Stream (Stream, streamRST)
import qualified Step.Cursor.Stream as Stream

import Step.Cursor.AdvanceResult (AdvanceResult)

import Step.RST

data CursorR xs x r s m =
  forall s'. CursorR
    { initR :: RST r s m s'
    , visibleStateLensR :: Lens' s' s
    , inputR :: Stream r s' m xs x
    }

rebaseCursorR :: Monad m1 => (forall a. m1 a -> m2 a) -> CursorR xs x r s m1 -> CursorR xs x r s m2
rebaseCursorR o CursorR{ initR, inputR, visibleStateLensR } =
  CursorR
    { initR = hoist o initR
    , visibleStateLensR
    , inputR = over streamRST (hoist o) inputR
    }

instance Contravariant (CursorR xs x r s m) (CursorR xs x r' s m) r r' where
    contramap f CursorR{ initR, inputR, visibleStateLensR } =
      CursorR
        { initR = contramap f initR
        , visibleStateLensR
        , inputR = contramap f inputR
        }

data CursorRW xs x r s m =
  forall s'. CursorRW
    { initRW :: RST r s m s'
    , visibleStateLensRW :: Lens' s' s
    , inputRW :: Stream r s' m xs x
    , commitRW :: Positive Natural -> RST r s' m AdvanceResult
    }

rebaseCursorRW :: Monad m1 => (forall a. m1 a -> m2 a) -> CursorRW xs x r s m1 -> CursorRW xs x r s m2
rebaseCursorRW o CursorRW{ initRW, commitRW, inputRW, visibleStateLensRW } =
  CursorRW
    { initRW = hoist o initRW
    , visibleStateLensRW
    , commitRW = hoist o . commitRW
    , inputRW = over streamRST (hoist o) inputRW
    }

instance Contravariant (CursorRW xs x r s m) (CursorRW xs x r' s m) r r' where
    contramap f CursorRW{ initRW, commitRW, inputRW, visibleStateLensRW } =
      CursorRW
        { initRW = contramap f initRW
        , visibleStateLensRW
        , inputRW = contramap f inputRW
        , commitRW = contramap f . commitRW
        }

data RunCursorRW xs x r s m =
    forall s'. RunCursorRW
      { inputRunRW :: Stream r s' m xs x
      , commitRunRW :: Positive Natural -> RST r s' m AdvanceResult
      , runRW :: forall a. RST r s' m a -> RST r s m a
      }

runCursorRW :: Monad m => CursorRW xs x r s m -> RunCursorRW xs x r s m
runCursorRW CursorRW{ initRW, visibleStateLensRW, inputRW, commitRW } =
  RunCursorRW
    { inputRunRW = inputRW
    , commitRunRW = commitRW
    , runRW = \a -> do
        r <- ask
        s <- initRW
        (x, s') <- lift (runRST a r s)
        put (view visibleStateLensRW s')
        return x
    }
