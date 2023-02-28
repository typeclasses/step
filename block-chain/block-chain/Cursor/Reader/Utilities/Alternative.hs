module Cursor.Reader.Utilities.Alternative where

import Essentials
import Cursor.Reader.Type

import Cursor.Interface (Mode (..))
import Data.Either (Either (..))
import Data.Foldable (foldMap)
import Control.Monad.Except (ExceptT, throwError)

import qualified Cursor.Atom as Atom

firstJust :: ExceptT error (ReaderPlus up action 'Write item block) (Maybe product)
    -> ReaderPlus up action 'Write item block (Maybe product)
firstJust = foldMap (Atom.atomically . fmap (maybe (Left ()) Right)) >>> Atom.optional

repetition :: ExceptT error (ReaderPlus up action 'Write item block) (Maybe product)
    -> Reader action 'Write item block [product]
repetition = fmap (maybe (Left ()) Right) >>> Atom.atomically >>> Atom.repetition
