{-# language GADTs, RankNTypes, TypeFamilies, NamedFieldPuns #-}

module Step.Input.Cursor where

import Step.Internal.Prelude

import Step.Nontrivial.Base

import Step.Input.AdvanceResult

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

instance (Monad m, Cursor m) => Cursor (ReaderT r m) where
    type Text (ReaderT r m) = Text m
    type Char (ReaderT r m) = Char m
    curse = rebaseSession lift (curse @m)
