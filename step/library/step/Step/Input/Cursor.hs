{-# language GADTs #-}

module Step.Input.Cursor where

import Step.Internal.Prelude hiding (while)

import Step.Nontrivial.Base

import Step.Input.AdvanceResult

import Step.Input.Stream

class Cursor m where
    type Text m :: Type
    type Char m :: Type
    curse :: Session (Text m) (Char m) m

data Session text char m =
  forall m'. Monad m' => Session
    { run :: forall a. m' a -> m a
    , commit :: Positive Natural -> m' AdvanceResult
    , input :: Stream m' (Nontrivial text char)
    }

rebaseSession :: (forall x. m1 x -> m2 x) -> Session text char m1 -> Session text char m2
rebaseSession f Session{ run, commit, input } =
    Session{ commit = commit, input = input, run = f . run }

---

data While text char = While

while :: forall m text char. Monad m => ListLike text char =>
    (char -> Bool) -> Session text char m -> Session text char m
while ok Session{ run = runUpstream :: forall a. m' a -> m a, commit = commitUpstream, input = inputUpstream } =
    Session{ run = run', commit = commit', input = input' }
  where
    run' :: StateT (While text char) m a -> m a
    run' = runUpstream . _

    commit' :: Positive Natural -> StateT (While text char) m AdvanceResult
    commit' n = _

    input' :: Stream (StateT (While text char) m) (Nontrivial text char)
    input' = _
