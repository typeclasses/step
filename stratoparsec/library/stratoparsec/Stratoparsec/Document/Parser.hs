module Stratoparsec.Document.Parser where

import ListT (ListT)

import qualified Stratoparsec.Stream.Base as Stream

import Stratoparsec.Document.ParseState (ParseState)
import qualified Stratoparsec.Document.ParseState as ParseState

data Error = Error{ errorContext :: [Context] }
    deriving stock (Eq, Show)

newtype Context = Context Text
    deriving newtype (Eq, Show, IsString)

defaultErrorOptions :: ErrorOptions
defaultErrorOptions = ErrorOptions{ errorLinesBefore = 4, errorLinesAfter = 2 }

data ErrorOptions = ErrorOptions{ errorLinesBefore :: Natural, errorLinesAfter :: Natural }

newtype ParserT m a = Parser (ErrorOptions -> StateT (ParseState m) m (Either Error a))
    deriving stock Functor
    deriving (Applicative, Monad)
        via (ReaderT ErrorOptions (ExceptT Error (StateT (ParseState m) m)))

type Parser a = forall m. Monad m => ParserT m a

parse :: Monad m => ErrorOptions -> ParserT m a -> StateT (ListT m Text) m (Either Error a)
parse eo (Parser p) = StateT \xs -> do
    (result, s) <- runStateT (p eo) (ParseState.start xs)
    return (result, Stream.toListT (ParseState.future s))

parseOnly :: Monad m => ErrorOptions -> ParserT m a -> ListT m Text -> m (Either Error a)
parseOnly eo p xs = evalStateT (parse eo p) xs
