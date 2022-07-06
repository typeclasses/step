-- | Summary of all the safe (not possible to construct invalid actions) parts of the `Step.Action.(...)` modules.
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
    -- * Noncommittal, Try
    Noncommittal, Try, try,
    -- * Can be static
    CanBeStatic, trivial,
    -- * Lift
    ActionLift, actionLiftTo,
    -- * Join
    ActionJoin, actionJoin, KindJoin, (:>),
    -- * Loop
    Loop0, Loop1,
    -- * Kinds
    ActionKind,
    Any, Static, Move, Undo, MoveUndo,
    Sure, SureStatic, SureMove,
  )
  where

import Step.Action.Config
import Step.Action.Failure
import Step.Action.Functor
import Step.Action.Join
import Step.Action.KindJoin
import Step.Action.Kinds
import Step.Action.Lift
import Step.Action.Loop
import Step.Action.SeparateTypes
import Step.Action.UnifiedType
