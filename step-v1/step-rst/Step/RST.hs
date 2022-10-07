{-# language DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, DerivingVia, StandaloneDeriving #-}

{- |

In addition to what is in this module, much of the functionality of 'RST' resides within class methods:

  * 'MonadTrans' ('lift')
  * 'MonadReader' ('ask')
  * 'MonadState' ('get', 'put')
  * 'Zoom' ('zoom')
  * 'MFunctor' ('hoist')

-}
module Step.RST
  (
    RST (..),
    {- * Running -} evalRST, execRST, runRST_,
    {- * Isomorphisms -} rst, rstState,
    {- * Modification -} stateRST, changeStateRST,
  )
  where

import Step.Internal.Prelude

import Optics

-- | Action with a read-only @r@ context, a mutable @s@ context, a base monad @m@, and a returned @a@
newtype RST r s m a = RST{ runRST :: r -> s -> m (a, s) }

-- | Simple isomorphism that just applies and removes the RST constructor
rst :: Iso (RST r1 s1 m1 a1) (RST r2 s2 m2 a2) (r1 -> s1 -> m1 (a1, s1)) (r2 -> s2 -> m2 (a2, s2))
rst = coerced

-- | Like 'runRST', but discards the state and only gives the result
evalRST :: Functor m => RST r s m a -> r -> s -> m a
evalRST = runRST' (view _1)

-- | Like 'runRST', but discards the result and only gives the state
execRST :: Functor m => RST r s m a -> r -> s -> m s
execRST = runRST' (view _2)

runRST' :: Functor f => ((a, s) -> b) -> RST r s f a -> r -> s -> f b
runRST' f a r s = f <$> runRST a r s

runRST_ :: Functor m => RST r s m a -> r -> s -> m ()
runRST_ a r s = void (runRST a r s)

-- | Lifts a state action into an RST transformation that disregards the reader context
stateRST :: StateT s m a -> RST r s m a
stateRST (StateT f) = RST \_ -> f

-- | Lifts a state transformation into an RST transformation that disregards the reader context
restateRST :: (StateT s1 m1 a1 -> StateT s2 m2 a2) -> RST r s1 m1 a1 -> RST r s2 m2 a2
restateRST f = mapRST (runStateT . f . StateT)

changeStateRST :: Functor m => (s1 -> s2) -> (s2 -> s1) -> RST r s1 m a1 -> RST r s2 m a1
changeStateRST a b = restateRST (\(StateT g) -> StateT \s -> g (b s) <&> over _2 a)

-- | Apply a change that ignores the reader context
mapRST :: ((s1 -> m1 (a1, s1)) -> s2 -> m2 (a2, s2)) -> RST r2 s1 m1 a1 -> RST r2 s2 m2 a2
mapRST f = over rst (f .)

rstState :: Iso (RST r s m a) (RST r s m a) (r -> StateT s m a) (r -> StateT s m a)
rstState = iso runRST (RST) % coerced

instance Contravariant (RST r s m a) (RST r' s m a) r r' where
    contramap f = over rst (. f)

deriving stock instance Functor m => Functor (RST r s m)
deriving via ReaderT r (StateT s m) instance Monad m => Applicative (RST r s m)
deriving via ReaderT r (StateT s m) instance Monad m => Monad (RST r s m)
deriving via ReaderT r (StateT s m) instance Monad m => MonadState s (RST r s m)
deriving via ReaderT r (StateT s m) instance Monad m => MonadReader r (RST r s m)
deriving via ComposeT (ReaderT r) (StateT s) instance MonadTrans (RST r s)
deriving via ComposeT (ReaderT r) (StateT s) instance MFunctor (RST r s)

instance Monad m => Zoom (RST r s m) (RST r t m) s t where
    zoom = restateRST . zoom
    zoomMaybe = restateRST . zoomMaybe
    zoomMany = restateRST . zoomMany
