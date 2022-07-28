{-# language FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Step.Buffer.Counting where

import Step.Internal.Prelude

import Step.Input.CursorPosition (CursorPosition)

import Step.Cursor (Cursor (Cursor), Stream, AdvanceResult, RST (..))
import qualified Step.Cursor as Cursor
import qualified Step.Input.CursorPosition as CursorPosition

import Optics

countingCursor :: forall xs x r s s' m. Monad m =>
    Cursor xs x r s s' m -> Cursor xs x r (CursorPosition, s) (CursorPosition, s') m
countingCursor c = Cursor{ Cursor.init, Cursor.input, Cursor.commit, Cursor.extract }
  where
    init (p, x) = (p, Cursor.init c x)

    input :: Stream (RST r (CursorPosition, s) m) xs x
    input = Cursor.rebaseStream (zoom _2) $ Cursor.input c

    commit :: Positive Natural -> RST r (CursorPosition, s) m AdvanceResult
    commit n = do
        modifying _1 (CursorPosition.strictlyIncrease n)
        zoom _2 (Cursor.commit c n)

    extract (p, x) = (p, Cursor.extract c x)
