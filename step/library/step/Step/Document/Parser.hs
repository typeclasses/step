module Step.Document.Parser where

import Step.Internal.Prelude

import Step.DocumentMemory.Base (DocumentMemory)
import qualified Step.DocumentMemory.Base as DocumentMemory

data Error text = Error{ errorContext :: [Context text] }
    deriving stock (Eq, Show)

newtype Context text = Context text
    deriving newtype (Eq, Show, IsString)

-- | A `Parser` can:
--
-- * Buffer new input
-- * Move the cursor forward
-- * Either fail by returning an 'Error', or succeed by returning an `a`
--
newtype Parser text m a = Parser (StateT (DocumentMemory text m) m (Either (Error text) a))
    deriving stock Functor
    deriving (Applicative, Monad)
        via (ExceptT (Error text) (StateT (DocumentMemory text m) m))

parse :: Monad m => Parser text m a -> StateT (DocumentMemory text m) m (Either (Error text) a)
parse (Parser p) = p

parseOnly :: Monad m => ListLike text Char => Parser text m a -> ListT m text -> m (Either (Error text) a)
parseOnly p xs = evalStateT (parse p) (DocumentMemory.fromListT xs)

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
