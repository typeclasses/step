module Stratoparsec.Parser.Type where

import Stratoparsec.Context (Context)
import Stratoparsec.BufferedStream.Type (BufferedStream)

type Parser :: (Type -> Type) -> Type -> Type -> Type -> Type

data ParseState m chunk buffer =
  ParseState
    { consumedInput :: buffer
    , remainingInput :: BufferedStream m chunk buffer
    }

newtype Parser m chunk buffer a =
  Parser
    (Context ->
        (StateT (ParseState m chunk buffer) m a)
    )

deriving stock
    instance Monad m => Functor (Parser m chunk buffer)

deriving via ReaderT Context (StateT (ParseState m chunk buffer) m)
    instance Monad m => Applicative (Parser m chunk buffer)

deriving via ReaderT Context (StateT (ParseState m chunk buffer) m)
    instance Monad m => Monad (Parser m chunk buffer)
