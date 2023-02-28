module Cursor.Atom.Examples
  (
    atom, right, just,
  )
  where

import Essentials

import Cursor.Interface (Mode (..))
import Cursor.Interface.Orders (commitNatural)
import Cursor.Atom.Type (AtomPlus (..))
import Cursor.Reader.Type (Reader, ReaderPlus (..))
import Cursor.Reader.Utilities.LookAhead (lookAhead)
import Cursor.Reader.Utilities.With (withLength)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, liftEither)
import Control.Monad.Trans (lift)
import Data.Either (Either (..))

atom :: forall up action item block error product.
    ExceptT error (Reader action 'Write item block) product
    -> AtomPlus up action item block error product
atom x = Atom do
    (len, ei) <- lift $ lookAhead $ withLength $ runExceptT x
    product <- liftEither ei
    pure $ Reader $ commitNatural len $> product

right :: forall up action item block error product.
    Reader action 'Write item block (Either error product)
    -> AtomPlus up action item block error product
right = ExceptT >>> atom

just :: forall up action item block product.
    Reader action 'Write item block (Maybe product)
    -> AtomPlus up action item block () product
just = fmap (maybe (Left ()) Right) >>> ExceptT >>> atom
