{-# language FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Step.Buffer.Counting where

import Step.Internal.Prelude

import Step.Input.CursorPosition (CursorPosition)

import Step.Cursor (Cursor (Cursor), Stream, AdvanceResult, streamRST)
import qualified Step.Cursor as Cursor
import qualified Step.Input.CursorPosition as CursorPosition

import Step.RST (RST (..))

import Optics

countingCursor :: forall xs x r s m. Monad m =>
    Cursor xs x r s m -> Cursor xs x r (CursorPosition, s) m
countingCursor
    Cursor
      { Cursor.init = init' :: s -> s'
      , Cursor.extract = extract'
      , Cursor.input = input'
      , Cursor.commit = commit'
      } =
    Cursor{ Cursor.init, Cursor.input, Cursor.commit, Cursor.extract }
  where
    init :: (CursorPosition, s) -> (CursorPosition, s')
    init = over _2 init'

    extract :: (CursorPosition, s') -> (CursorPosition, s)
    extract = over _2 extract'

    input :: Stream r (CursorPosition, s') m xs x
    input = over streamRST (zoom _2) input'

    commit :: Positive Natural -> RST r (CursorPosition, s') m AdvanceResult
    commit n = do
        modifying _1 (CursorPosition.strictlyIncrease n)
        zoom _2 (commit' n)
