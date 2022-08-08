module Step.RunAction where

import Step.Internal.Prelude

import Step.Cursor
import qualified Step.Cursor as Cursor

import Step.RST

import Step.ActionTypes

runQuery :: forall xs x r s s' m a. Monad m => CursorRunR xs x r s s' m -> Query xs x r s r m a -> RST r s m (Either r a)
runQuery CursorRunR{ inputRunR, runR, resetRunR } = runR . r
  where
    r :: forall a'. Query xs x r s r m a' -> RST r (CursorState s s') m (Either r a')
    r = \case
        Query_Lift x -> Right <$> lift x
        Query_Ask f -> Right . f <$> ask
        Query_Get f -> Right . f <$> use commitLens
        Query_Next f -> Right . f <$> Cursor.next inputRunR
        Query_Join x -> r x >>= either (return . Left) (\y -> resetRunR *> r y)
        Query_Fail f -> Left . f <$> ask

runAny :: forall xs x r s s' e m a. Monad m => CursorRunRW xs x r s s' m -> Any xs x r s e m a -> RST r s m (Either e a)
runAny CursorRunRW{ inputRunRW, runRW, commitRunRW, resetRunRW } = runRW . r
  where
    r :: forall a'. Any xs x r s e m a' -> RST r (CursorState s s') m (Either e a')
    r = \case
        Any_Lift x -> Right <$> lift x
        Any_Ask f -> Right . f <$> ask
        Any_Get f -> Right . f <$> use commitLens
        Any_Next f -> Right . f <$> Cursor.next inputRunRW
        Any_Join x -> r x >>= either (return . Left) (\y -> resetRunRW *> r y)
        Any_Fail f -> Left . f <$> ask
        Any_Commit n x -> Right x <$ commitRunRW n
