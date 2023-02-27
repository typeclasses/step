module Reset.Interface.Class
  (
    {- * Class -} ResetStream (..),
    {- * Requests -} next, reset,
  )
  where

import Reset.Interface.Type
import Next.Interface

class TerminableStream item interface => ResetStream item interface where
    liftReset :: Reset item result -> interface result

instance ResetStream item (Reset item) where
    liftReset x = x

reset :: ResetStream item interface => interface ()
reset = liftReset Reset
