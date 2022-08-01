{-# language FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Step.Buffer.Counting where

import Step.Internal.Prelude

import Step.Input.CursorPosition (CursorPosition)

import Step.Cursor (ReadWriteCursor (ReadWriteCursor), Stream, AdvanceResult, streamRST)
import qualified Step.Cursor as Cursor
import qualified Step.Input.CursorPosition as CursorPosition

import Step.RST (RST (..))

import qualified Optics

countingCursor :: forall xs x r s m. Monad m =>
    Lens' s CursorPosition
    -> ReadWriteCursor xs x r s m
    -> ReadWriteCursor xs x r s m
countingCursor positionLens
    ReadWriteCursor
      { Cursor.init = init' :: s -> s'
      , Cursor.input = input'
      , Cursor.commit = commit'
      } =
    ReadWriteCursor{ Cursor.init, Cursor.input, Cursor.commit }
  where
    init = init'
    input = input'

    commit :: Positive Natural -> RST r (s', s) m AdvanceResult
    commit n = do
        modifying (Optics._2 % positionLens) (CursorPosition.strictlyIncrease n)
        commit' n
