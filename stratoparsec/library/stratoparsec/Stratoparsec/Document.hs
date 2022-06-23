module Stratoparsec.Document where

import ListT (ListT)

import Stratoparsec.Buffer (Buffer)
import Stratoparsec.Stream (Stream)

import qualified Stratoparsec.Buffer as Buffer
import qualified Stratoparsec.Stream as Stream

data Error = Error{ errorContext :: [Context] }

newtype Context = Context Text

data Position = Position{ line :: Natural, column :: Natural }

beginning :: Position
beginning = Position 0 0

data ParseState m =
  ParseState
    { past :: Buffer Text
    , future :: Stream m Text
    , contextStack :: [Context]
    , position :: Position
    }

initialParseState :: ListT m Text -> ParseState m
initialParseState xs = ParseState{ past = Buffer.empty, future = Stream.fromListT xs, contextStack = [], position = beginning }

data ErrorOptions = ErrorOptions{ errorLinesBefore :: Natural, errorLinesAfter :: Natural }

newtype Parser m a = Parser (ErrorOptions -> StateT (ParseState m) m (Either Error a))
    deriving stock Functor
    deriving (Applicative, Monad)
        via (ReaderT ErrorOptions (ExceptT Error (StateT (ParseState m) m)))

parse :: Monad m => ErrorOptions -> Parser m a -> StateT (ListT m Text) m (Either Error a)
parse eo (Parser p) = StateT \xs -> do
    (result, s) <- runStateT (p eo) (initialParseState xs)
    return (result, Stream.toListT (future s))

parseOnly :: Monad m => ErrorOptions -> Parser m a -> ListT m Text -> m (Either Error a)
parseOnly eo p xs = evalStateT (parse eo p) xs
