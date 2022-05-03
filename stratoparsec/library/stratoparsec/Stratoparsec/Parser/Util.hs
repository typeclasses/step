module Stratoparsec.Parser.Util where

import Stratoparsec.Context.Type
import Stratoparsec.Name
import Stratoparsec.Parser.Type

import qualified Stratoparsec.Context.Util as Context

withContext :: (Context -> Context) -> Parser m chunk buffer a -> Parser m chunk buffer a
withContext f (Parser p) = Parser \c -> p (f c)

-- | Name the parser, in case failure occurs
--
-- @'named' = 'flip' ('<?>')@
named :: Name -> Parser m chunk buffer a -> Parser m chunk buffer a
named n = withContext (Context.push n)

-- | Name the parser, in case failure occurs
--
-- @('<?>') = 'flip' 'named'@
(<?>) :: Parser m chunk buffer a -> Name -> Parser m chunk buffer a
(<?>) = flip named
