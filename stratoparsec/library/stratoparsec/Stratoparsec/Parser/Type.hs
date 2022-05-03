module Stratoparsec.Parser.Type where

import Stratoparsec.Context (Context)
import Stratoparsec.BufferedStream.Type (BufferedStream)

import ListT

type Parser :: (Type -> Type) -> Type -> Type -> Type -> Type

newtype Parser m chunk buffer a =
  Parser
    (Context ->
        (StateT (BufferedStream m chunk buffer) (ListT m) a)
    )

deriving stock
    instance Monad m => Functor (Parser m chunk buffer)

deriving via ReaderT Context (StateT (BufferedStream m chunk buffer) (ListT m))
    instance Monad m => Applicative (Parser m chunk buffer)

deriving via ReaderT Context (StateT (BufferedStream m chunk buffer) (ListT m))
    instance Monad m => Monad (Parser m chunk buffer)
