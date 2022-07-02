module Step.Document.Parser where

import Step.Internal.Prelude

import Step.DocumentMemory.Base (DocumentMemory)
import qualified Step.DocumentMemory.Base as DocumentMemory

import Step.Document.Config (Config)

import Step.Document.Error (Error)

-- | A `Parser` can:
--
-- * Buffer new input
-- * Move the cursor forward
-- * Either fail by returning an 'Error', or succeed by returning an `a`
--
newtype Parser text m a = Parser (Config text -> StateT (DocumentMemory text m) m (Either (Error text) a))
    deriving stock Functor
    deriving (Applicative, Monad)
        via (ReaderT (Config text) (ExceptT (Error text) (StateT (DocumentMemory text m) m)))

parse :: Monad m => Config text -> Parser text m a -> StateT (DocumentMemory text m) m (Either (Error text) a)
parse config (Parser p) = p config

parseOnly :: Monad m => ListLike text Char => Config text -> Parser text m a -> ListT m text -> m (Either (Error text) a)
parseOnly config p xs = evalStateT (parse config p) (DocumentMemory.fromListT xs)

-- | A `Possibility` can:
--
-- * Buffer input
-- * Either fail (without moving the cursor), or succeed (move the cursor forward, return an `a`)
--
newtype Possibility text m a = Possibility (StateT (DocumentMemory text m) m (Maybe a))
    deriving stock Functor

-- newtype Transform text m text' =
--     Transform (ListT (StateT (BufferedStream m text) m) text')
--     deriving stock Functor
