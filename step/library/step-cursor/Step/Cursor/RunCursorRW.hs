{-# language FlexibleInstances, FunctionalDependencies, GADTs #-}

module Step.Cursor.RunCursorRW
  (
    runCursorRW,
    RunCursorRW (..),
  )
  where

import Step.Internal.Prelude

import Step.Cursor.Stream (Stream, streamRST)
import qualified Step.Cursor.Stream as Stream

import Step.Cursor.AdvanceResult (AdvanceResult)

import Step.RST

import qualified Step.Cursor.CursorRW as C

data RunCursorRW xs x r s m =
    forall s'. RunCursorRW
      { input :: Stream r s' m xs x
      , commit :: Positive Natural -> RST r s' m AdvanceResult
      , run :: forall a. RST r s' m a -> RST r s m a
      }

runCursorRW :: Monad m => C.CursorRW xs x r s m -> RunCursorRW xs x r s m
runCursorRW C.CursorRW{ C.init, C.visibleStateLens, C.input, C.commit } =
  RunCursorRW
    { input = input
    , commit = commit
    , run = \a -> do
        r <- ask
        s <- init
        (x, s') <- lift (runRST a r s)
        put (view visibleStateLens s')
        return x
    }
