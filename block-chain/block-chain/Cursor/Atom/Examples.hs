module Cursor.Atom.Examples
  (
    atomically,
  )
  where

import Essentials

import Cursor.Interface (Mode (..))
import Cursor.Interface.Orders (commitNatural)
import Cursor.Atom.Type (AtomPlus (..))
import Cursor.Reader.Type (Reader, ReaderPlus (..))
import Cursor.Reader.Utilities.LookAhead (lookAhead)
import Cursor.Reader.Utilities.With (withLength)
import Control.Monad.Except (ExceptT, runExceptT, liftEither)
import Control.Monad.Trans (lift)

atomically :: forall up action item block error product.
    ExceptT error (Reader action 'Write item block) product
    -> AtomPlus up action item block error product
atomically x = Atom do
    (len, ei) <- lift $ lookAhead $ withLength $ runExceptT x
    product <- liftEither ei
    pure $ Reader $ commitNatural len $> product
