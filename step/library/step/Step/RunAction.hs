{-# language DataKinds #-}

module Step.RunAction where

import Step.Internal.Prelude

import Step.Cursor
import qualified Step.Cursor as Cursor

import Step.RST

import Step.Action

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
runAny CursorRunRW{ inputRunRW, runRW, resetRunRW, commitRunRW } (Any (Walk a)) =
      runRW (runFree @a inputRunRW resetRunRW commitRunRW a)

runFree :: forall a m xs x r s s'. Monad m =>
    Stream r (CursorState s s') m xs x
    -> RST r (CursorState s s') m () -> (Positive Natural
    -> RST r (CursorState s s') m AdvanceResult)
    -> Free (Step 'ReadWrite 'Imperfect xs x r (RST r s m)) a
    -> RST r (CursorState s s') m (Either r a)
runFree inputRunRW resetRunRW commitRunRW = r
  where
    r :: Free (Step 'ReadWrite 'Imperfect xs x r (RST r s m)) a -> RST r (CursorState s s') m (Either r a)
    r = \case
      Pure x -> return (Right x)
      Free b -> case b of
          Step_Lift x -> zoom commitLens x >>= r
          Step_Commit (Commit n x) -> commitRunRW n *> r x
          Step_Reset x -> resetRunRW *> r x
          Step_Fail (Error x) -> return (Left x)
          Step_Next f -> Cursor.next inputRunRW >>= r . f
