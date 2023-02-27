module Cursor.Reader.Repetition where

import Essentials
import Cursor.Reader.Type
import Cursor.Interface

import Data.Either (Either (..))

repetition ::
    AtomPlus up action item block error product
    -> ReaderPlus up action 'Write item block [product]
repetition (Atom x) = recur
  where
    recur = do
      z <- x
      case z of
          Left _ -> pure []
          Right rw -> do
              a <- rw
              (a :) <$> recur
