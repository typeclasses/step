module Step.RunAction where

import Step.Internal.Prelude

import Step.Cursor
import qualified Step.Cursor as Cursor

import Step.RST

import Step.Action

runQuery :: forall xs x r s s' m a. Monad m => CursorRunR xs x r s s' m -> Query xs x r s r m a -> RST r s m (Either r a)
runQuery CursorRunR{ inputRunR, runR, resetRunR } (Query (FreeAction a)) = runR (runFree a)
    where
    runFree :: forall a'. Free (Base xs x r s r m) a' -> RST r (CursorState s s') m (Either r a')
    runFree = \case
        Pure x -> return (Right x)
        Free b -> case b of
            Base_RST x -> zoom commitLens x >>= runFree
            Base_Reset x -> resetRunR *> runFree x
            Base_Fail (Fail x) -> ask <&> Left . x
            Base_Next f -> Cursor.next inputRunR >>= runFree . f

runAny :: forall xs x r s s' m a. Monad m => CursorRunRW xs x r s s' m -> Any xs x r s r m a -> RST r s m (Either r a)
runAny CursorRunRW{ inputRunRW, runRW, resetRunRW, commitRunRW } (Any (FreeAction a)) = runRW (runFree a)
    where
    runFree :: forall a'. Free (BaseRW xs x r s r m) a' -> RST r (CursorState s s') m (Either r a')
    runFree = \case
        Pure x -> return (Right x)
        Free b -> case b of
            BaseRW_Commit n x -> commitRunRW n *> runFree x
            BaseRW_Base b' -> case b' of
                Base_RST x -> zoom commitLens x >>= runFree
                Base_Reset x -> resetRunRW *> runFree x
                Base_Fail (Fail x) -> ask <&> Left . x
                Base_Next f -> Cursor.next inputRunRW >>= runFree . f
