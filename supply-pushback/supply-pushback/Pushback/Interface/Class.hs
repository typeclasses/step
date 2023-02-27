module Pushback.Interface.Class
  (
    {- * Class -} PushbackStream (..), TerminableStream (..),
    {- * Requests -} next, push,
  )
  where

import Pushback.Interface.Type
import Next.Interface

class TerminableStream item interface => PushbackStream item interface where
    liftPushback :: Pushback item result -> interface result

instance PushbackStream item (Pushback item) where
    liftPushback x = x

push :: PushbackStream item interface => item -> interface ()
push x = liftPushback (Push x)
