module Step.Document.Parser where

import ListT (ListT)

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

newtype ParserT text m a = Parser (ErrorOptions -> StateT (ParseState text m) m (Either (Error text) a))
    deriving stock Functor
    deriving (Applicative, Monad)
        via (ReaderT ErrorOptions (ExceptT (Error text) (StateT (ParseState text m) m)))

type Parser text a = forall m. Monad m => ParserT text m a

parse :: Monad m => ErrorOptions -> ParserT text m a -> StateT (ListT m text) m (Either (Error text) a)
parse eo (Parser p) = StateT \xs -> do
    (result, s) <- runStateT (p eo) (ParseState.start xs)
    return (result, Stream.toListT (ParseState.future s))

parseOnly :: Monad m => ErrorOptions -> ParserT text m a -> ListT m text -> m (Either (Error text) a)
parseOnly eo p xs = evalStateT (parse eo p) xs
