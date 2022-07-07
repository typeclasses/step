-- | Summary of all the safe (not possible to construct invalid actions) parts of the @Step.Action.(...)@ modules.
--
module Step.Action.Safe
  (
    IsAction,
    -- * Failure
    CanFail, failure,
    -- * Functor
    FunctorAction, MonadAction,
    -- * Configure
    ConfigurableAction, configureAction,
    -- * Always moves
    AlwaysMoves,
    -- * Atomic, Try
    Atomic, Try, try,
    -- * Can be static
    CanBeStatic, trivial,
    -- * Lift
    Is, cast,
    -- * Join
    ActionJoin, actionJoin, KindJoin, (:>),
    -- * Loop
    Loop0, Loop1,
    -- * Kinds
    ActionKind,
    Any, Query, Move, Atom, AtomicMove,
    Sure, SureQuery,
  )
  where

import Step.Action.AlwaysMoves
import Step.Action.Atomic
import Step.Action.CanBeStatic
import Step.Action.Config
import Step.Action.Failure
import Step.Action.Functor
import Step.Action.IsAction
import Step.Action.Join
import Step.Action.KindJoin
import Step.Action.Kinds
import Step.Action.Loop
import Step.Action.Subtyping
