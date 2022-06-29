module Step.Document.Parser where

import Step.Internal.Prelude

import Step.BufferedStream.Base (BufferedStream)

import Step.CountingBufferedStream.Base (CountingBufferedStream)
import qualified Step.CountingBufferedStream.Base as CountingBufferedStream

import Step.DocumentMemory.Base (DocumentMemory)
import qualified Step.DocumentMemory.Base as DocumentMemory

data Error text = Error{ errorContext :: [Context text] }
    deriving stock (Eq, Show)

newtype Context text = Context text
    deriving newtype (Eq, Show, IsString)

defaultConfig :: Config
defaultConfig = Config

data Config = Config

-- | A `Parser` can:
--
-- * Modify the 'ParseState', restricted to valid modifications -- it can buffer new input, and it can move input from the future to the past
-- * Either fail by returning an 'Error', or succeed by returning an `a`
--
newtype Parser text m a = Parser (Config -> StateT (CountingBufferedStream m text) m (Either (Error text) a))
    deriving stock Functor
    deriving (Applicative, Monad)
        via (ReaderT Config (ExceptT (Error text) (StateT (CountingBufferedStream m text) m)))

parse :: Monad m => Config -> Parser text m a -> StateT (DocumentMemory text m) m (Either (Error text) a)
parse config (Parser p) = do
    dm <- get
    (r, dm') <- lift (runStateT (parse' (p config)) dm)
    put dm'
    return r

parse' :: Monad m => StateT (CountingBufferedStream m text) m a -> StateT (DocumentMemory text m) m a
parse' go = do
    dm <- get
    let cbs = _
    _

parseOnly :: Monad m => Config -> Parser text m a -> ListT m text -> m (Either (Error text) a)
parseOnly eo p xs = evalStateT (parse eo p) (DocumentMemory.fromListT xs)

-- | A `Possibility` can:
--
-- * Modify the future, restricted to the buffering of new input
-- * Either fail, or succeed (move input from the future to the past, return an `a`)
--
newtype Possibility text m a = Possibility (Config -> StateT (CountingBufferedStream m text) m (Maybe a))
    deriving stock Functor

newtype Transform text m text' =
    Transform (ListT (StateT (BufferedStream m text) m) text')
    deriving stock Functor
