module Cursor.Reader.Optional where

import Essentials
import Cursor.Reader.Type
import Cursor.Interface

import Data.Either (Either (..))

optional ::
    AtomPlus up action item block error product
    -> ReaderPlus up action 'Write item block (Maybe product)
optional (Atom x) = do
    z <- x
    case z of
        Left _ -> pure Nothing
        Right rw -> Just <$> rw
