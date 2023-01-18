module Pushback.Interface.Class
  (
    {- * Class -} PushbackStream (..),
    {- * Requests -} next, push,
  )
  where

import Pushback.Interface.Type
import Next.Interface

class PushbackStream item interface where
    liftPushback :: Pushback item result -> interface result

push :: PushbackStream item interface => item -> interface ()
push x = liftPushback (Push x)
