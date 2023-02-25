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
import Data.Either (Either (..))

atomically ::
    Reader action 'Write item block (Either error product)
    -> AtomPlus up action item block error product
atomically x = Atom do
    (len, ei) <- lookAhead (withLength x)
    pure do
        product <- ei
        pure $ Reader do
            _ <- commitNatural len
            pure product
