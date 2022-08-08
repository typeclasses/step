{-# language FlexibleContexts, QualifiedDo #-}

module Step.ActionTypes.GeneralActions
  (
    {- * Character -} takeCharMaybe, takeChar, nextChar, nextCharMaybe, satisfyJust,
    {- * Chunk -} next, nextMaybe, takeNext, takeNextMaybe,
    {- * End -} atEnd, end,
    {- * Commit -} commit,
    {- * General -} actionState, actionContext,
    {- * Fail -} fail,
  )
  where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors
import Step.ActionTypes.Atomic
import Step.ActionTypes.Subtyping

import qualified Step.ActionTypes.Do as A

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial

import Positive.Unsafe (Positive (PositiveUnsafe))

commit :: Monad m => Positive Natural -> AtomicMove xs x r s e m ()
commit n = AtomicMove (Atom (return (Sure (Any_Commit n ()))))

fail :: Fail xs x r s r m a
fail = Fail id

takeCharMaybe :: Monad m => Sure xs x r s e m (Maybe x)
takeCharMaybe = try takeChar

takeChar :: Monad m => AtomicMove xs x r s r m x
takeChar = nextChar A.<* commit one

nextChar :: Monad m => Query xs x r s r m x
nextChar = nextCharMaybe A.>>= maybe (cast @Query fail) return

nextMaybe :: Monad m => SureQuery xs x r s e m (Maybe (Nontrivial xs x))
nextMaybe = SureQuery (Query_Next id)

next :: Monad m => Query xs x r s r m (Nontrivial xs x)
next = nextMaybe A.>>= maybe (cast @Query fail) return

takeNext :: Monad m => AtomicMove xs x r s r m (Nontrivial xs x)
takeNext = next A.>>= \xs -> commit (Nontrivial.length xs) $> xs

takeNextMaybe :: Monad m => Sure xs x r s e m (Maybe (Nontrivial xs x))
takeNextMaybe = try takeNext

nextCharMaybe :: Monad m => SureQuery xs x r s e m (Maybe x)
nextCharMaybe = nextMaybe A.<&> fmap @Maybe Nontrivial.head

satisfyJust :: Monad m => (x -> Maybe a) -> AtomicMove xs x r s r m a
satisfyJust ok = nextCharMaybe A.>>= \x -> case x >>= ok of Nothing -> cast fail; Just y -> commit one $> y

atEnd :: Monad m => SureQuery xs x r s e m Bool
atEnd = SureQuery (Query_Next isNothing)

end :: Monad m => Query xs x r s r m ()
end = atEnd A.>>= \case{ True -> return (); False -> cast @Query fail }

actionState :: SureQuery xs x r s e m s
actionState = SureQuery (Query_Get id)

actionContext :: SureQuery xs x r s e m r
actionContext = SureQuery (Query_Ask id)

one :: Positive Natural
one = PositiveUnsafe 1

-- while :: Monad m => LossOfMovement act1 act2 => Nontrivial.GeneralSpanOperation xs x
--     -> act1 xs x r s e m a -> act2 xs x r s e m a
-- while = _


-- todo: add an atomic version of 'text'

-- text :: Nontrivial xs x -> Move xs x r s m ()
-- text = someOfNontrivialText A.>=> (maybe (return ()) (cast @Any . text) . Nontrivial.refine)
--   where
--     someOfNontrivialText x = Action.Unsafe.AtomicMove $ case curse of
--         CursorRW{ init, input, commit } -> run $ Cursor.next input >>= \case
--             Nothing -> return (Left F.failure)
--             Just y ->
--                 if x `Nontrivial.isPrefixOf` y
--                 then commit (Nontrivial.length x) $> Right ListLike.empty
--                 else
--                 if y `Nontrivial.isPrefixOf` x
--                 then commit (Nontrivial.length y) $>
--                       Right
--                         (
--                           ListLike.drop
--                               (ListLike.length (Nontrivial.generalize y))
--                               (Nontrivial.generalize x)
--                         )
--                 else return (Left F.failure)
