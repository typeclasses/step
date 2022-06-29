module Step.Document.Parser where

import Step.Internal.Prelude

import Step.Stream.Base (Stream)
import qualified Step.Stream.Base as Stream

import Step.Document.ParseState (ParseState)
import qualified Step.Document.ParseState as ParseState

data Error text = Error{ errorContext :: [Context text] }
    deriving stock (Eq, Show)

newtype Context text = Context text
    deriving newtype (Eq, Show, IsString)

defaultErrorOptions :: ErrorOptions
defaultErrorOptions = ErrorOptions{ errorLinesBefore = 4, errorLinesAfter = 2 }

data ErrorOptions = ErrorOptions{ errorLinesBefore :: Natural, errorLinesAfter :: Natural }

-- | A `Parser` can:
--
-- * Modify the 'ParseState', restricted to valid modifications -- it can buffer new input, and it can move input from the future to the past
-- * Either fail by returning an 'Error', or succeed by returning an `a`
--
newtype Parser text m a = Parser (ErrorOptions -> StateT (ParseState text m) m (Either (Error text) a))
    deriving stock Functor
    deriving (Applicative, Monad)
        via (ReaderT ErrorOptions (ExceptT (Error text) (StateT (ParseState text m) m)))

parse :: Monad m => ErrorOptions -> Parser text m a -> StateT (ListT m text) m (Either (Error text) a)
parse eo (Parser p) = StateT \xs -> do
    (result, s) <- runStateT (p eo) (ParseState.start xs)
    return (result, Stream.toListT (ParseState.future s))

parseOnly :: Monad m => ErrorOptions -> Parser text m a -> ListT m text -> m (Either (Error text) a)
parseOnly eo p xs = evalStateT (parse eo p) xs

-- | A `Possibility` can:
--
-- * Modify the future, restricted to the buffering of new input
-- * Either fail, or succeed (move input from the future to the past, return an `a`)
--
newtype Possibility text m a =
  Possibility
    (
      ErrorOptions
      -> ParseState text m
      -> m
        (Either
          (Stream m text) -- Rejected -- returns a new 'future' stream that is equivalent to the current future (but may be buffered more)
          (ParseState text m, a) -- Accepted -- returns a new state and parsed value
        )
    )
    deriving stock Functor

newtype Transform text m text' =
  Transform
    (
      ListT (StateT (Stream m text) m) text'
    )
  deriving stock Functor
