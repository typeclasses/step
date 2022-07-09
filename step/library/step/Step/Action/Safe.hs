{-# language ExplicitNamespaces, Safe #-}

-- | Summary of all the safe (not possible to construct invalid actions) parts of the @Step.Action.(...)@ modules.

module Step.Action.Safe
  (
    -- * Failure
    CanFail, failure,
    -- * Functor, Monad
    FunctorialAction, MonadicAction,
    -- * Configure
    ConfigurableAction, configureAction,
    -- * Atomic, try
    Atomic, try,
    -- * Can be static
    CanBeStatic, trivial,
    -- * Lift
    Is, cast,
    -- * Join
    Join, join, type (>>),
    -- * Loop
    Loop0, Loop1,
    -- * Kinds
    ActionKind,
    Any, Query, Move, Atom, AtomicMove,
    Sure, SureQuery,
  )
  where

import Step.Action.Atomic
import Step.Action.CanBeStatic
import Step.Action.Config
import Step.Action.Failure
import Step.Action.Functorial
import Step.Action.Join
import Step.Action.KindJoin
import Step.Action.Loop
import Step.Action.Monadic
import Step.Action.Subtyping
import Step.Action.Types
