module Cursor.Reader.Utilities.Exception where

import Essentials
import Cursor.Reader.Type

import Data.Bool (bool)
import Control.Monad.Trans (lift)
import Control.Monad.Except (ExceptT, throwError)

true :: e -> ReaderPlus up action mode item block Bool
    -> ExceptT e (ReaderPlus up action mode item block) ()
true e r = lift r >>= bool (throwError e) (pure ())

just :: e -> ReaderPlus up action mode item block (Maybe a)
    -> ExceptT e (ReaderPlus up action mode item block) a
just e r = lift r >>= maybe (throwError e) pure
