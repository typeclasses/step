{-# language FlexibleInstances, FunctionalDependencies, GADTs #-}

{-

"reset" operation repositions the input stream to the point of the last commit.

-}

module Step.Cursor.Cursor
  (
    -- * Two states
    CursorState (..), commitLens, sessionLens,

    -- * The backend interface for building cursors
    -- Cursor (..),
    CursorR (..), CursorRW (..),
    -- Cursor' (..),
    CursorR' (..), CursorRW' (..),

    -- * The user interface for running cursors
    CursorRunR (..), cursorRunR,
    CursorRunRW (..), cursorRunRW,

    -- * Conversion
    readOnly, readOnly'
  )
  where

import Step.Internal.Prelude

import Step.Cursor.Stream (Stream, streamRST)
import qualified Step.Cursor.Stream as Stream

import Step.Cursor.AdvanceResult (AdvanceResult)

import Step.RST

data CursorState s s' =
  CursorState
    { commitState :: s
    , sessionState :: s'
    }

commitLens :: Lens (CursorState s1 s') (CursorState s2 s') s1 s2
commitLens = lens commitState \x y -> x{ commitState = y }

sessionLens :: Lens (CursorState s s'1) (CursorState s s'2) s'1 s'2
sessionLens = lens sessionState \x y -> x{ sessionState = y }

-- data Cursor' xs x r s m =
--   Cursor'
--     { cursorR' :: CursorR' xs x r s m
--     , cursorRW' :: CursorRW' xs x r s m
--     }

data CursorR' xs x r s m =
  forall s'. CursorR' (CursorR xs x r s s' m)

data CursorRW' xs x r s m =
  forall s'. CursorRW' (CursorRW xs x r s s' m)

instance Contravariant (CursorR' xs x r s m) (CursorR' xs x r' s m) r r' where
  contramap f (CursorR' a) = CursorR' (contramap f a)

instance Contravariant (CursorRW' xs x r s m) (CursorRW' xs x r' s m) r r' where
  contramap f (CursorRW' a) = CursorRW' (contramap f a)

-- instance Contravariant (Cursor' xs x r s m) (Cursor' xs x r' s m) r r' where
--   contramap f (Cursor' a b) = Cursor' (contramap f a) (contramap f b)

-- data Cursor xs x r s sr srw m =
--   Cursor
--     { cursorR  :: CursorR  xs x r s sr  m
--     , cursorRW :: CursorRW xs x r s srw m
--     }

-- instance Contravariant (Cursor xs x r s sr srw m) (Cursor xs x r' s sr srw m) r r' where
--   contramap f Cursor{ cursorR, cursorRW } =
--     Cursor
--       { cursorR = contramap f cursorR
--       , cursorRW = contramap f cursorRW
--       }

data CursorR xs x r s s' m =
  CursorR
    { initR :: RST r s m s'
    , inputR :: Stream r (CursorState s s') m xs x
    , resetR :: RST r (CursorState s s') m ()
    }

instance Contravariant (CursorR xs x r s s' m) (CursorR xs x r' s s' m) r r' where
    contramap f c =
      CursorR
        { initR = contramap f (initR c)
        , inputR = contramap f (inputR c)
        , resetR = contramap f (resetR c)
        }

data CursorRW xs x r s s' m =
  CursorRW
    { initRW :: RST r s m s'
    , inputRW :: Stream r (CursorState s s') m xs x
    , commitRW :: Positive Natural -> RST r (CursorState s s') m AdvanceResult
    , resetRW :: RST r (CursorState s s') m ()
    }

instance Contravariant (CursorRW xs x r s s' m) (CursorRW xs x r' s s' m) r r' where
    contramap f c =
      CursorRW
        { initRW = contramap f (initRW c)
        , inputRW = contramap f (inputRW c)
        , commitRW = contramap f . (commitRW c)
        , resetRW = contramap f (resetRW c)
        }

data CursorRunR xs x r s s' m =
    CursorRunR
      { inputRunR :: Stream r (CursorState s s') m xs x
      , resetRunR :: RST r (CursorState s s') m ()
      , runR :: forall a. RST r (CursorState s s') m a -> RST r s m a
      }

cursorRunR :: Monad m => CursorR xs x r s s' m -> CursorRunR xs x r s s' m
cursorRunR CursorR{ initR, inputR, resetR } =
  CursorRunR
    { inputRunR = inputR
    , resetRunR = resetR
    , runR = \a -> do
        r <- ask
        s <- get
        s' <- initR
        (x, ss) <- lift (runRST a r (CursorState s s'))
        put (view commitLens ss)
        return x
    }

data CursorRunRW xs x r s s' m =
    CursorRunRW
      { inputRunRW :: Stream r (CursorState s s') m xs x
      , commitRunRW :: Positive Natural -> RST r (CursorState s s') m AdvanceResult
      , resetRunRW :: RST r (CursorState s s') m ()
      , runRW :: forall a. RST r (CursorState s s') m a -> RST r s m a
      }

cursorRunRW :: Monad m => CursorRW xs x r s s' m -> CursorRunRW xs x r s s' m
cursorRunRW CursorRW{ initRW, inputRW, commitRW, resetRW } =
  CursorRunRW
    { inputRunRW = inputRW
    , commitRunRW = commitRW
    , resetRunRW = resetRW
    , runRW = \a -> do
        r <- ask
        s <- get
        s' <- initRW
        (x, ss) <- lift (runRST a r (CursorState s s'))
        put (view commitLens ss)
        return x
    }

readOnly :: CursorRW xs x r s s' m -> CursorR xs x r s s' m
readOnly c =
  CursorR
    { initR = initRW c
    , inputR = inputRW c
    , resetR = resetRW c
    }

readOnly' :: CursorRW' xs x r s m -> CursorR' xs x r s m
readOnly' (CursorRW' c) =
  CursorR' CursorR
    { initR = initRW c
    , inputR = inputRW c
    , resetR = resetRW c
    }
