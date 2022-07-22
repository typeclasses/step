{-# language GADTs, RankNTypes, TypeFamilies, NamedFieldPuns #-}

module Step.Input.Cursor where

import Step.Internal.Prelude

import Step.Nontrivial.Base

import Step.Input.AdvanceResult

import Optics (_1, _2, zoom, use, assign)

class Cursor m where
    type Text m :: Type
    type Char m :: Type
    curse :: Session (Text m) (Char m) m

data Session text char m =
  forall m'. Monad m' => Session
    { run :: forall a. m' a -> m a
    , commit :: Positive Natural -> m' AdvanceResult
    , next :: m' (Maybe (Nontrivial text char))
    }

rebaseSession :: (forall x. m1 x -> m2 x) -> Session text char m1 -> Session text char m2
rebaseSession f Session{ run, commit, next } =
    Session{ commit = commit, next = next, run = f. run }

-- stateSession :: forall s m text char. Monad m =>
--     StateT s m (Maybe (Nontrivial text char)) -- ^ next
--     -> (Positive Natural -> StateT s m AdvanceResult) -- ^ commit
--     -> Session text char (StateT s m)
-- stateSession next' commit' =
--     Session{ run, commit, next }
--   where
--     run :: StateT (s, s) m a -> StateT s m a
--     run a = do
--         b <- get
--         (x, (_, b')) <- lift (runStateT a (b, b))
--         put b'
--         return x

--     next :: StateT (s, s) m (Maybe (Nontrivial text char))
--     next = zoom _1 next'

--     commit :: Positive Natural -> StateT (s, s) m AdvanceResult
--     commit n = do
--         s <- use _2
--         (x, s') <- lift (runStateT (commit' n) s)
--         assign _2 s'
--         return x

instance (Monad m, Cursor m) => Cursor (ReaderT r m) where
    type Text (ReaderT r m) = Text m
    type Char (ReaderT r m) = Char m
    curse = rebaseSession lift (curse @m)

-- data While m text char
