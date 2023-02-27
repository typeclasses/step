module Pushback.Interface
  (
    {- * Types -} Pushback (..), Step (..),
    {- * Class -} PushbackStream (..), TerminableStream (..), next, push,
  )
  where

import Pushback.Interface.Type
import Pushback.Interface.Class
