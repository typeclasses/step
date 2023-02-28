module Cursor.Atom.Utilities
  (
    optional, repetition
  )
  where

import Essentials
import Cursor.Atom.Type
import Cursor.Reader.Type
import Cursor.Interface

import Data.Either (Either (..))

optional :: AtomPlus up action item block error product
    -> ReaderPlus up action 'Write item block (Either error product)
optional (Atom x) = do
    z <- x
    case z of
        Left e -> pure (Left e)
        Right rw -> Right <$> rw

repetition :: Monoid product => AtomPlus up action item block error product
    -> ReaderPlus up action 'Write item block (error, product)
repetition (Atom x) = recur
  where
    recur = do
      z <- x
      case z of
          Left e -> pure (e, mempty)
          Right rw -> do
              a <- rw
              (e, as) <- recur
              pure (e, a <> as)
