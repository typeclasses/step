{-# language DataKinds #-}

module Step.RunAction where

import Step.Internal.Prelude

import Step.Cursor
import qualified Step.Cursor as Cursor

import Step.RST

import Step.Action

import qualified Monad

-- runQuery :: forall xs x r s s' m a. Monad m => CursorRunR xs x r s s' m -> Query xs x r s r m a -> RST r s m (Either r a)
-- runQuery CursorRunR{ inputRunR, runR, resetRunR } (Query (FreeAction a)) = runR (runFree a)
--     where
--     runFree :: forall a'. Free (Base xs x r s r m) a' -> RST r (CursorState s s') m (Either r a')
--     runFree = \case
--         Pure x -> return (Right x)
--         Free b -> case b of
--             Step_Lift x -> zoom commitLens x >>= runFree
--             Step_Reset x -> resetRunR *> runFree x
--             Step_Fail (Fail x) -> ask <&> Left . x
--             Step_Next f -> Cursor.next inputRunR >>= runFree . f

runAny :: forall a m xs x r s s'. Monad m => CursorRunRW xs x r s s' m -> Any xs x r (RST r s m) a -> RST r s m (Either r a)
runAny CursorRunRW{ inputRunRW, runRW, resetRunRW, commitRunRW } (Any (Walk aaa)) =
    runRW (r aaa)
  where
    r :: forall a'. F (Step 'ReadWrite 'Imperfect xs x r (RST r s m)) a' -> RST r (CursorState s s') m (Either r a')
    r a = runF a f g
      where
        f :: a' -> RST r (CursorState s s') m (Either r a')
        f = return . Right

        g :: Step 'ReadWrite 'Imperfect xs x r (RST r s m) (RST r (CursorState s s') m (Either r a')) -> RST r (CursorState s s') m (Either r a')
        g = \case
              Step_Lift x -> Monad.join $ zoom commitLens x
              Step_Commit (Commit n x) -> Monad.join $ commitRunRW n $> x
              Step_Reset x -> Monad.join $ resetRunRW $> x
              Step_Fail (Error x) -> return (Left x)
              Step_Next x -> Monad.join $ Cursor.next inputRunRW <&> x
