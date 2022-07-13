{-# language FlexibleContexts, FunctionalDependencies, QualifiedDo, Safe, TypeFamilies, TypeOperators #-}

module Step.ActionTypes.Loop where

import Step.Internal.Prelude

import Step.ActionTypes.Returnable
import Step.ActionTypes.Types
import Step.ActionTypes.Functorial
import Step.ActionTypes.Join
import Step.ActionTypes.KindJoin
import Step.ActionTypes.Subtyping
import Step.ActionTypes.Atomic

import qualified Step.ActionTypes.Do as P

import qualified NonEmpty

-- | Loop0 k k' means that a repetition of 0 or more k actions results in a k' action.
class (Join k k', Returnable k', Is (k >> k') k') => Loop0 k k' | k -> k'

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

-- | Loop1 k k' means that a repetition of 1 or more k actions results in a k' action.
class (Join k k', Is k k', Is (k >> k') k') => Loop1 k k' | k -> k'

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

count0 :: Monad m => Loop0 k k' => Natural -> k e m a -> k' e m [a]
count0 = \n a -> go a n
  where
    go a = fix \r -> \case
        0 -> trivial []
        n -> cast ((:) P.<$> a P.<*> (r (n - 1)))

count1 :: Monad m => Loop1 k k' => Positive Natural -> k e m a -> k' e m (NonEmpty a)
count1 = \n a -> go a n
  where
    go a = fix \r -> \p ->
        case preview positive (review positive p - 1) of
            Nothing -> (:| []) <$> cast a
            Just p' -> cast (NonEmpty.cons P.<$> a P.<*> r p')

repetition0 :: Monad m => AtomicMove e m a -> Sure e m [a]
repetition0 p = fix \r -> P.do
    xm <- try p
    case xm of
        Nothing -> return []
        Just x -> (x :) <$> r

repetition1 :: Monad m => AtomicMove e m a -> AtomicMove e m (NonEmpty a)
repetition1 p = P.do
    x <- p
    xs <- repetition0 p
    P.return (x :| xs)
