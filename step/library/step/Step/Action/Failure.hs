module Step.Action.Failure where

import Step.Internal.Prelude

import Step.Action.Kinds

import qualified Step.Action.Coerce as Action

class CanFail (k :: ActionKind)
  where
    failure :: Monad m => (config -> StateT cursor m error) -> k config cursor error m a

instance CanFail Any      where failure = view Action.coerced . failureAny
instance CanFail Static   where failure = view Action.coerced . failureAny
instance CanFail Move     where failure = view Action.coerced . failureAny
instance CanFail Undo     where failure = view Action.coerced . failureAny
instance CanFail MoveUndo where failure = view Action.coerced . failureAny

failureAny :: Monad m => (config -> (StateT cursor m error)) -> Any config cursor error m a
failureAny f = Any \c -> return (Left (f c))