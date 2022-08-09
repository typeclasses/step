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
        Query_Join x -> r x >>= either (return . Left) r
        Query_Base b -> case b of
            Base_Lift x -> Right <$> lift x
            Base_Ask f -> Right . f <$> ask
            Base_Get f -> Right . f <$> use commitLens
            Base_Next f -> Right . f <$> Cursor.next inputRunR
            Base_Fail f -> Left . f <$> ask
            Base_Reset x -> Right x <$ resetRunR

runAny :: forall xs x r s s' e m a. Monad m => CursorRunRW xs x r s s' m -> Any xs x r s e m a -> RST r s m (Either e a)
runAny CursorRunRW{ inputRunRW, runRW, commitRunRW, resetRunRW } = runRW . r
  where
    r :: forall a'. Any xs x r s e m a' -> RST r (CursorState s s') m (Either e a')
    r = \case
        Any_Join x -> r x >>= either (return . Left) r
        Any_Commit n x -> Right x <$ commitRunRW n
        Any_Base b -> case b of
            Base_Lift x -> Right <$> lift x
            Base_Ask f -> Right . f <$> ask
            Base_Get f -> Right . f <$> use commitLens
            Base_Next f -> Right . f <$> Cursor.next inputRunRW
            Base_Fail f -> Left . f <$> ask
            Base_Reset x -> Right x <$ resetRunRW
