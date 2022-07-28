{-# language FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, DerivingVia #-}

module Step.Cursor.Type
  (
    Cursor (..),
    rebase,
    -- recurse,
    {- * RST -} RST (..), evalRST, mapRST,
  )
  where

import Step.Internal.Prelude

import Step.Cursor.ChunkStream (Stream)
import qualified Step.Cursor.ChunkStream as Stream
import Step.Cursor.AdvanceResult (AdvanceResult)

import Functor.Compose

import Optics

newtype RST r s m a = RST{ runRST :: r -> s -> m (a, s) }
    deriving
        ( Functor, Applicative, Monad
        , MonadReader r, MonadState s
        ) via ReaderT r (StateT s m)
    deriving MonadTrans
        via (ComposeT (ReaderT r) (StateT s))

instance Monad m => Zoom (RST r s m) (RST r t m) s t where
    zoom      o (RST f) = RST \r -> runStateT (zoom      o $ StateT (f r))
    zoomMaybe o (RST f) = RST \r -> runStateT (zoomMaybe o $ StateT (f r))
    zoomMany  o (RST f) = RST \r -> runStateT (zoomMany  o $ StateT (f r))

evalRST :: Functor m => RST r s m a -> r -> s -> m a
evalRST a r s = (\(x, _) -> x) <$> runRST a r s

mapRST :: (forall x. m1 x -> m2 x) -> RST r s m1 a -> RST r s m2 a
mapRST f = RST . ((f .) .) . runRST

data Cursor xs x r s s' m =
  Cursor
    { init :: RST r s' m s
    , input :: Stream (RST r s m) xs x
    , commit :: Positive Natural -> RST r s m AdvanceResult
    , extract :: RST r s m s'
    }

rebase :: (forall a. m1 a -> m2 a) -> Cursor xs x r s s' m1 -> Cursor xs x r s s' m2
rebase o Cursor{ init, commit, input, extract } =
  Cursor
    { init = mapRST o init
    , commit = mapRST o . commit
    , input = Stream.rebase (mapRST o) input
    , extract = mapRST o extract
    }

-- recurse :: (forall a. Iso' (cursor1 a) (cursor2 a)) -> Cursor xs x base cursor1 -> Cursor xs x base cursor2
-- recurse i Cursor{ run, commit, input } =
--     Cursor{ commit = \n -> view i $ commit n, input = Stream.rebase (view i) input, run = run . review i }
