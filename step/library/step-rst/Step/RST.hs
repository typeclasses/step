{-# language FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, DerivingVia, ViewPatterns #-}

module Step.RST
  (
    RST (..),
    {- * Running -} evalRST,
    {- * Modification -} contramapRST,
  )
  where

import Step.Internal.Prelude

import Optics

import Functor.Compose

newtype RST r s m a = RST{ runRST :: r -> s -> m (a, s) }
    deriving
        (
            Functor,
            Applicative,
            Monad,
            MonadReader r,
            MonadState s
        )
        via ReaderT r (StateT s m)
    deriving
        (
            MonadTrans,
            MFunctor
        )
        via ComposeT (ReaderT r) (StateT s)

instance Monad m => Zoom (RST r s m) (RST r t m) s t where
    zoom = changeStateRST . zoom
    zoomMaybe = changeStateRST . zoomMaybe
    zoomMany = changeStateRST . zoomMany

changeStateRST :: (StateT s1 m1 a1 -> StateT s2 m2 a2) -> RST r s1 m1 a1 -> RST r s2 m2 a2
changeStateRST f = RST . ((runStateT . f . StateT) .) . runRST

evalRST :: Functor m => RST r s m a -> r -> s -> m a
evalRST a r s = (\(x, _) -> x) <$> runRST a r s

contramapRST :: Monad m => (r' -> r) -> RST r s m a -> RST r' s m a
contramapRST f (RST g) = RST (g . f)
