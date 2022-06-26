module Stratoparsec.Document.Parser where

import Optics

import ListT (ListT)

import Stratoparsec.Stream.Base (Stream)
import Stratoparsec.Document.Past (Past)
import Stratoparsec.Document.Position (Position)

import qualified Stratoparsec.Stream.Base as Stream
import qualified Stratoparsec.Document.Position as Position
import qualified Stratoparsec.Document.Past as Past

data Error = Error{ errorContext :: [Context] }
    deriving stock (Eq, Show)

newtype Context = Context Text
    deriving newtype (Eq, Show, IsString)

data ParseState m =
  ParseState
    { past :: Past
    , future :: Stream m Text
    , position :: Position
    }

makeLensesFor [("future", "futureLens")] ''ParseState

initialParseState :: ListT m Text -> ParseState m
initialParseState xs =
    ParseState{
        past = Past.empty,
        future = Stream.fromListT xs,
        position = Position.start
    }

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
    (result, s) <- runStateT (p eo) (initialParseState xs)
    return (result, Stream.toListT (future s))

parseOnly :: Monad m => ErrorOptions -> ParserT m a -> ListT m Text -> m (Either Error a)
parseOnly eo p xs = evalStateT (parse eo p) xs
