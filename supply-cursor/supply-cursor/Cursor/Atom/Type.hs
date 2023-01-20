module Cursor.Atom.Type where

import Cursor.Interface
import Essentials
import SupplyChain
import Cursor.Reader.Type

import Control.Applicative (Alternative, empty, (<|>))

newtype Atom action block alt product =
    Atom (Reader action 'Read block (alt (Reader action 'Write block product)))

-- ?????????
