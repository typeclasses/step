{-# language GADTs #-}

module Step.Input.Cursor where

import Step.Internal.Prelude hiding (while)

import Step.Nontrivial.Base

import Step.Input.AdvanceResult

import Step.Input.Stream
import qualified Step.Input.Stream as Stream

import Step.Buffer.Base (Buffer)
import qualified Step.Buffer.Base as Buffer

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
