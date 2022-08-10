module Step.RunAction where

import Step.Internal.Prelude

import Step.Cursor
import qualified Step.Cursor as Cursor

import Step.RST

import Step.ActionTypes

runQuery :: forall xs x r s s' m a. Monad m => CursorRunR xs x r s s' m -> Query xs x r s r m a -> RST r s m (Either r a)
runQuery CursorRunR{ inputRunR, runR, resetRunR } = runR . r . (\(Query q) -> q)
  where
    r :: forall a'. Joining Base xs x r s r m a' -> RST r (CursorState s s') m (Either r a')
    r = \case
        Join x -> r x >>= either (return . Left) r
        Plain b -> case b of
            Base_Lift x -> Right <$> lift x
            Base_Ask f -> Right . f <$> ask
            Base_Get f -> Right . f <$> use commitLens
            Base_Next f -> Right . f <$> Cursor.next inputRunR
            Base_Fail f -> Left . f <$> ask
            Base_Reset x -> Right x <$ resetRunR

runAny :: forall xs x r s s' e m a. Monad m => CursorRunRW xs x r s s' m -> Any xs x r s e m a -> RST r s m (Either e a)
runAny CursorRunRW{ inputRunRW, runRW, commitRunRW, resetRunRW } = runRW . r . (\(Any a) -> a)
  where
    r :: forall a'. Joining BaseRW xs x r s e m a' -> RST r (CursorState s s') m (Either e a')
    r = \case
        Join x -> r x >>= either (return . Left) r
        Plain b -> case b of
            BaseRW_Commit n x -> Right x <$ commitRunRW n
            BaseRW_Base c -> case c of
                Base_Lift x -> Right <$> lift x
                Base_Ask f -> Right . f <$> ask
                Base_Get f -> Right . f <$> use commitLens
                Base_Next f -> Right . f <$> Cursor.next inputRunRW
                Base_Fail f -> Left . f <$> ask
                Base_Reset x -> Right x <$ resetRunRW
