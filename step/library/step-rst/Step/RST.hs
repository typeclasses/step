{-# language FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, DerivingVia #-}

module Step.RST
  (
    RST (..),
    evalRST, mapRST,
  )
  where

import Step.Internal.Prelude

import Optics

import Functor.Compose

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
