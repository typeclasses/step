{-# language FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Step.Buffer.Counting where

import Step.Internal.Prelude

import Step.Input.CursorPosition (CursorPosition)

import Step.Cursor (Cursor (Cursor), Stream, AdvanceResult, streamRST)
import qualified Step.Cursor as Cursor
import qualified Step.Input.CursorPosition as CursorPosition

import Step.RST (RST (..))

import qualified Optics

countingCursor :: forall xs x r s m. Monad m =>
    Lens' s CursorPosition
    -> Cursor xs x r s m
    -> Cursor xs x r s m
countingCursor positionLens
    Cursor
      { Cursor.init = init' :: s -> s'
      , Cursor.extract = extract'
      , Cursor.input = input'
      , Cursor.commit = commit'
      } =
    Cursor{ Cursor.init, Cursor.input, Cursor.commit, Cursor.extract }
  where
    init = init'
    extract = extract'
    input = input'

    commit :: Positive Natural -> RST r (s', s) m AdvanceResult
    commit n = do
        modifying (Optics._2 % positionLens) (CursorPosition.strictlyIncrease n)
        commit' n
