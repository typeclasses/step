{-# language FlexibleContexts, QualifiedDo #-}

module Step.ActionTypes.GeneralActions
  (
    {- * Character -} takeCharMaybe, takeChar, nextChar, nextCharMaybe, satisfyJust,
    {- * Chunk -} next, nextMaybe, takeNext, takeNextMaybe,
    {- * Misc. -} skip, skip0, skipAtomically, skipAtomically0,
    {- * End -} atEnd, end,
    {- * Commit -} commit,
    {- * General -} actionState, actionContext,
    {- * Fail -} fail,
    {- * Loop -} count0, count1, repetition0, repetition1,
  )
  where

import Step.Internal.Prelude

import Step.ActionTypes.Constructors

import qualified Step.ActionTypes.Do as A

import Step.Nontrivial (Nontrivial)
import qualified Step.Nontrivial as Nontrivial

import Positive.Unsafe (Positive (PositiveUnsafe))
import qualified Positive
import qualified Positive.Math as Positive
import qualified Signed

import qualified NonEmpty

commit :: Monad m => Positive Natural -> AtomicMove xs x r s e m ()
commit n = assumeMovement $ castTo @Atom $ BaseRW_Commit n ()

fail :: Fail xs x r s r m a
fail = Fail id

takeCharMaybe :: Monad m => Sure xs x r s e m (Maybe x)
takeCharMaybe = mapError' $ try takeChar

takeChar :: Monad m => AtomicMove xs x r s r m x
takeChar = nextChar A.<* commit one

nextChar :: Monad m => Query xs x r s r m x
nextChar = nextCharMaybe A.>>= maybe (castTo @Query fail) return

nextMaybe :: Monad m => SureQuery xs x r s e m (Maybe (Nontrivial xs x))
nextMaybe = A.do{ reset; nextMaybe' }

-- | Like 'nextMaybe', but doesn't reset first
nextMaybe' :: Functor m => SureQuery xs x r s e m (Maybe (Nontrivial xs x))
nextMaybe' = cast $ SureBase $ Base_Next id

next :: Monad m => Query xs x r s r m (Nontrivial xs x)
next = nextMaybe A.>>= maybe (castTo @Query fail) return

-- | Like 'next', but doesn't reset first
next' :: Monad m => Query xs x r s r m (Nontrivial xs x)
next' = nextMaybe' A.>>= maybe (castTo @Query fail) return

takeNext :: Monad m => AtomicMove xs x r s r m (Nontrivial xs x)
takeNext = next A.>>= \xs -> commit (Nontrivial.length xs) $> xs

takeNextMaybe :: Monad m => Sure xs x r s e m (Maybe (Nontrivial xs x))
takeNextMaybe = mapError' $ try takeNext

nextCharMaybe :: Monad m => SureQuery xs x r s e m (Maybe x)
nextCharMaybe = nextMaybe A.<&> fmap @Maybe Nontrivial.head

satisfyJust :: Monad m => (x -> Maybe a) -> AtomicMove xs x r s r m a
satisfyJust ok = nextCharMaybe A.>>= \x -> case x >>= ok of Nothing -> castTo fail; Just y -> commit one $> y

skip0 :: Monad m => Natural -> Any xs x r s r m ()
skip0 = maybe (return ()) (castTo @Any . skip)  . preview Positive.refine

skip :: Monad m => Positive Natural -> Move xs x r s r m ()
skip n = A.do
    x <- next
    case Positive.minus (Nontrivial.length x) n of
        Signed.Minus n' -> A.do
            commit (Nontrivial.length x)
            skip n'
        _ -> castTo @Move (commit n)

skipAtomically0 :: Monad m => Natural -> Atom xs x r s r m ()
skipAtomically0 = maybe (castTo @Atom $ A.return ()) (castTo @Atom . skipAtomically)  . preview Positive.refine

skipAtomically :: Monad m => Positive Natural -> AtomicMove xs x r s r m ()
skipAtomically n = A.do{ ensureAtLeast n; commit n }

ensureAtLeast :: Monad m => Positive Natural -> Query xs x r s r m ()
ensureAtLeast = \n -> A.do{ castTo @Query reset; go n }
  where
    go :: Monad m => Positive Natural -> Query xs x r s r m ()
    go n = A.do
        x <- next'
        case Positive.minus n (Nontrivial.length x) of
            Signed.Plus n' -> go n'
            _ -> return ()

atEnd :: Monad m => SureQuery xs x r s e m Bool
atEnd = A.do{ reset; x <- nextMaybe'; A.return (isNothing x) }

end :: Monad m => Query xs x r s r m ()
end = A.do{ e <- atEnd; if e then return () else castTo @Query fail }

reset :: Monad m => SureQuery xs x r s e m ()
reset = cast $ SureBase $ Base_Reset ()

actionState :: Monad m => SureQuery xs x r s e m s
actionState = cast $ SureBase $ Base_RST get

actionContext :: Monad m => SureQuery xs x r s e m r
actionContext = cast $ SureBase $ Base_RST ask

one :: Positive Natural
one = PositiveUnsafe 1

-- while :: Monad m => LossOfMovement act1 act2 => Nontrivial.GeneralSpanOperation xs x
--     -> act1 xs x r s e m a -> act2 xs x r s e m a
-- while = _


-- todo: add an atomic version of 'text'

-- text :: Nontrivial xs x -> Move xs x r s m ()
-- text = someOfNontrivialText A.>=> (maybe (return ()) (castTo @Any . text) . Nontrivial.refine)
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

count0 :: forall act1 act2 xs x r s e m a. Monad m =>
    Loop0 act1 act2 => Natural -> act1 xs x r s e m a -> act2 xs x r s e m [a]
count0 = \n a -> go a n
  where
    go a = fix \r -> \case
        0 -> trivial []
        n -> castTo @act2 ((:) A.<$> a A.<*> (r (n - 1)))

count1 :: forall act1 act2 xs x r s e m a. Monad m => Loop1 act1 act2 =>
    Positive Natural -> act1 xs x r s e m a -> act2 xs x r s e m (NonEmpty a)
count1 = \n a -> go a n
  where
    go a = fix \r -> \p ->
        case preview positive (review positive p - 1) of
            Nothing -> (:| []) <$> castTo @act2 a
            Just p' -> castTo @act2 (NonEmpty.cons A.<$> a A.<*> r p')

repetition0 :: Monad m => AtomicMove xs x r s e m a -> Sure xs x r s e m [a]
repetition0 p = fix \r -> A.do
    xm <- try p
    case xm of
        Nothing -> return []
        Just x -> (x :) <$> r

repetition1 :: Monad m => AtomicMove xs x r s e m a -> AtomicMove xs x r s e m (NonEmpty a)
repetition1 p = A.do
    x <- p
    xs <- repetition0 p
    A.return (x :| xs)
