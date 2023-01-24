module Cursor.Atom.Utilities
  (
    optional, repetition,
  )
  where

import Essentials
import Cursor.Atom.Type
import Cursor.Reader.Type
import Cursor.Interface

import Data.Either (Either (..))

optional ::
    AtomPlus up action block error product
    -> ReaderPlus up action 'Write block (Maybe product)
optional (Atom x) = do
    z <- x
    case z of
        Left _ -> pure Nothing
        Right rw -> Just <$> rw

repetition ::
    AtomPlus up action block error product
    -> ReaderPlus up action 'Write block [product]
repetition (Atom x) = recur
  where
    recur = do
      z <- x
      case z of
          Left _ -> pure []
          Right rw -> do
              a <- rw
              (a :) <$> recur
