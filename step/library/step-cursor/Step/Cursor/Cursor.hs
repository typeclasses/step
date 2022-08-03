{-# language FlexibleInstances, FunctionalDependencies, GADTs #-}

module Step.Cursor.Cursor
  (
    -- * The backend interface for building cursors
    Cursor (..), CursorR (..), CursorRW (..),

    -- * The user interface for running cursors
    CursorRunR (..), cursorRunR,
    CursorRunRW (..), cursorRunRW,

    -- * Conversion
    readOnly,
  )
  where

import Step.Internal.Prelude

import Step.Cursor.Stream (Stream, streamRST)
import qualified Step.Cursor.Stream as Stream

import Step.Cursor.AdvanceResult (AdvanceResult)

import Step.RST

data Cursor xs x r s m =
  Cursor
    { cursorR  :: CursorR  xs x r s m
    , cursorRW :: CursorRW xs x r s m
    }

instance Contravariant (Cursor xs x r s m) (Cursor xs x r' s m) r r' where
  contramap f Cursor{ cursorR, cursorRW } =
    Cursor
      { cursorR = contramap f cursorR
      , cursorRW = contramap f cursorRW
      }

data CursorR xs x r s m =
  forall s'. CursorR
    { initR :: RST r s m s'
    , visibleStateLensR :: Lens' s' s
    , inputR :: Stream r s' m xs x
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

instance Contravariant (CursorRW xs x r s m) (CursorRW xs x r' s m) r r' where
    contramap f CursorRW{ initRW, commitRW, inputRW, visibleStateLensRW } =
      CursorRW
        { initRW = contramap f initRW
        , visibleStateLensRW
        , inputRW = contramap f inputRW
        , commitRW = contramap f . commitRW
        }

data CursorRunR xs x r s m =
    forall s'. CursorRunR
      { inputRunR :: Stream r s' m xs x
      , runR :: forall a. RST r s' m a -> RST r s m a
      }

cursorRunR :: Monad m => CursorR xs x r s m -> CursorRunR xs x r s m
cursorRunR CursorR{ initR, visibleStateLensR, inputR } =
  CursorRunR
    { inputRunR = inputR
    , runR = \a -> do
        r <- ask
        s <- initR
        (x, s') <- lift (runRST a r s)
        put (view visibleStateLensR s')
        return x
    }

data CursorRunRW xs x r s m =
    forall s'. CursorRunRW
      { inputRunRW :: Stream r s' m xs x
      , commitRunRW :: Positive Natural -> RST r s' m AdvanceResult
      , runRW :: forall a. RST r s' m a -> RST r s m a
      }

cursorRunRW :: Monad m => CursorRW xs x r s m -> CursorRunRW xs x r s m
cursorRunRW CursorRW{ initRW, visibleStateLensRW, inputRW, commitRW } =
  CursorRunRW
    { inputRunRW = inputRW
    , commitRunRW = commitRW
    , runRW = \a -> do
        r <- ask
        s <- initRW
        (x, s') <- lift (runRST a r s)
        put (view visibleStateLensRW s')
        return x
    }

readOnly :: CursorRW xs x r s m -> CursorR xs x r s m
readOnly CursorRW{ initRW, visibleStateLensRW, inputRW } =
  CursorR
    { initR = initRW
    , visibleStateLensR = visibleStateLensRW
    , inputR = inputRW
    }
