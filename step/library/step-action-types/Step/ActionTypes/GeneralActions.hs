{-# language FlexibleContexts, QualifiedDo #-}

module Step.ActionTypes.GeneralActions
  (
    {- * Character -} takeCharMaybe, takeChar, nextChar, nextCharMaybe,
    {- * Chunk -} next, nextMaybe,
    {- * End -} atEnd, end,
    {- * Commit -} commit,
    {- * General -} actionState, actionContext,
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

commit :: Monad m => Positive Natural -> AtomicMove xs x r s m ()
commit n = AtomicMove (Atom (return (Sure_Commit n ())))

fail :: Fail xs x r s m a
fail = Fail

takeCharMaybe :: Monad m => Sure xs x r s m (Maybe x)
takeCharMaybe = try takeChar

takeChar :: Monad m => AtomicMove xs x r s m x
takeChar = nextChar A.<* commit one

nextChar :: Monad m => Query xs x r s m x
nextChar = nextCharMaybe A.>>= maybe (cast @Query fail) return

nextMaybe :: Monad m => SureQuery xs x r s m (Maybe (Nontrivial xs x))
nextMaybe = SureQuery_Next id

next :: Monad m => Query xs x r s m (Nontrivial xs x)
next = nextMaybe A.>>= maybe (cast @Query fail) return

nextCharMaybe :: Monad m => SureQuery xs x r s m (Maybe x)
nextCharMaybe = nextMaybe A.<&> fmap @Maybe Nontrivial.head

atEnd :: Monad m => SureQuery xs x r s m Bool
atEnd = SureQuery_Next isNothing

end :: Monad m => Query xs x r s m ()
end = atEnd A.>>= \case{ True -> return (); False -> Query_Fail id }

actionState :: SureQuery xs x r s m s
actionState = SureQuery_Get id

actionContext :: SureQuery xs x r s m r
actionContext = SureQuery_Ask id

one :: Positive Natural
one = PositiveUnsafe 1
