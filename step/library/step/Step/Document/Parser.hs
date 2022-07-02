module Step.Document.Parser where

import Step.Internal.Prelude

import Step.DocumentMemory.Base (DocumentMemory)
import qualified Step.DocumentMemory.Base as DocumentMemory

import Step.Document.Config (Config)
import qualified Step.Document.Config as Config

import Step.Document.Error (Error (Error))
import qualified Step.Document.Error as Error

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

-- | Like 'Parser', but always consumes at least 1 character if it succeeds
--
newtype Parser1 text m a = Parser1 (Config text -> StateT (DocumentMemory text m) m (Either (Error text) a))
    deriving stock Functor
    deriving (Applicative, Monad) via (Parser text m)

-- | Like 'Parser', but cannot fail
--
newtype Always text m a = Always (Config text -> StateT (DocumentMemory text m) m a)
    deriving stock Functor
    deriving (Applicative, Monad)
        via (ReaderT (Config text) (StateT (DocumentMemory text m) m))

-- | Like 'Parser', but does not move the cursor if it fails, thus permitting backtracking
--
newtype Possibility text m a = Possibility (Config text -> StateT (DocumentMemory text m) m (Either (Error text) a))
    deriving stock Functor

-- | Like 'Possibility', but always consumes at least 1 character if it succeeds
newtype Possibility1 text m a = Possibility1 (Config text -> StateT (DocumentMemory text m) m (Either (Error text) a))
    deriving stock Functor

-- | Parser that always fails
data Failure text (m :: Type -> Type) a = Failure

-- newtype Transform text m text' =
--     Transform (ListT (StateT (BufferedStream m text) m) text')
--     deriving stock Functor

class Lift p1 p2
  where
    lift :: Monad m
         => p1 text (m :: Type -> Type) a
         -> p2 text (m :: Type -> Type) a

instance Lift Always Parser
  where
    lift (Always p) = Parser (\config -> Right <$> p config)

instance Lift Possibility Parser
  where
    lift = coerce

instance Lift Possibility1 Parser1
  where
    lift = coerce

instance Lift Possibility1 Possibility
  where
    lift = coerce

instance Lift Possibility1 Parser
  where
    lift = coerce

instance Lift Parser1 Parser
  where
    lift = coerce

instance Lift Failure Parser
  where
    lift Failure = Parser (\config -> return (Left (makeError config)))

instance Lift Failure Parser1
  where
    lift Failure = Parser1 (\config -> return (Left (makeError config)))

instance Lift Failure Possibility
  where
    lift Failure = Possibility (\config -> return (Left (makeError config)))

instance Lift Failure Possibility1
  where
    lift Failure = Possibility1 (\config -> return (Left (makeError config)))

instance Lift p p
  where
    lift = id

makeError :: Config text -> Error text
makeError config = Error{ Error.context = Config.context config }

parse :: Monad m => Config text -> Parser text m a -> StateT (DocumentMemory text m) m (Either (Error text) a)
parse config (Parser p) = p config

parseOnly :: Monad m => ListLike text Char => Config text -> Parser text m a -> ListT m text -> m (Either (Error text) a)
parseOnly config p xs = evalStateT (parse config p) (DocumentMemory.fromListT xs)
