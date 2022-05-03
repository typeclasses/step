module Stratoparsec.Context.Util where

import Stratoparsec.Context.Type
import Stratoparsec.Name

push :: Name -> Context -> Context
push n (Context c) = Context (n : c)
