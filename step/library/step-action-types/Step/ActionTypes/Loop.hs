{-# language FlexibleContexts, FunctionalDependencies, QualifiedDo, Safe, TypeOperators #-}

module Step.ActionTypes.Loop where

import Step.Internal.Prelude

import Step.ActionTypes.Returnable
import Step.ActionTypes.Types
import Step.ActionTypes.Join
import Step.ActionTypes.KindJoin
import Step.ActionTypes.Subtyping
import Step.ActionTypes.Atomic

import qualified Step.ActionTypes.Do as P

import qualified NonEmpty

-- | Loop0 act1 act2 means that a repetition of 0 or more act1 actions results in an act2 action.
class (Join act1 act2, Returnable act2, Is (act1 >> act2) act2) =>
    Loop0 act1 act2 | act1 -> act2

-- Atomic actions loose their atomicity when sequenced 2 or more times; guaranteed advancement is lost when sequencing 0 times

instance Loop0 Atom Any
instance Loop0 AtomicMove Any
instance Loop0 Move Any

-- Other kinds are preserved

instance Loop0 Any Any
instance Loop0 Sure Sure
instance Loop0 SureQuery SureQuery
instance Loop0 Query Query

---

-- | Loop1 act1 act2 means that a repetition of 1 or more act1 actions results in an act2 action.
class (Join act1 act2, Is act1 act2, Is (act1 >> act2) act2) =>
    Loop1 act1 act2 | act1 -> act2

-- Atomic actions loose their atomicity when sequenced 2 or more times

instance Loop1 Atom Any
instance Loop1 AtomicMove Move

-- All other kinds are preserved by sequencing

instance Loop1 Any Any
instance Loop1 Query Query
instance Loop1 Move Move
instance Loop1 Sure Sure
instance Loop1 SureQuery SureQuery

---

count0 :: Monad m => Loop0 act1 act2 => Natural -> act1 m e a -> act2 m e [a]
count0 = \n a -> go a n
  where
    go a = fix \r -> \case
        0 -> trivial []
        n -> cast ((:) P.<$> a P.<*> (r (n - 1)))

count1 :: Monad m => Loop1 act1 act2 => Positive Natural -> act1 m e a -> act2 m e (NonEmpty a)
count1 = \n a -> go a n
  where
    go a = fix \r -> \p ->
        case preview positive (review positive p - 1) of
            Nothing -> (:| []) <$> cast a
            Just p' -> cast (NonEmpty.cons P.<$> a P.<*> r p')

repetition0 :: Monad m => AtomicMove m e a -> Sure m e [a]
repetition0 p = fix \r -> P.do
    xm <- try p
    case xm of
        Nothing -> return []
        Just x -> (x :) <$> r

repetition1 :: Monad m => AtomicMove m e a -> AtomicMove m e (NonEmpty a)
repetition1 p = P.do
    x <- p
    xs <- repetition0 p
    P.return (x :| xs)
