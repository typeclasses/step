{-# language ExplicitNamespaces #-}

module Step.ActionTypes
  (
    -- * The types
    Action,
    -- $types
    Any (..), Query (..), Move (..), Atom (..), AtomicMove (..),
    Sure (..), SureQuery (..), Fail (..), Base (..), SureBase (..),
    -- * Subtype relationships
    -- $subtypes
    Is, castTo,
    -- * Monadic-style join
    Join, join, type (>>),
    -- * Miscellaneous classes
    -- ** Functorial, Monadic
    FunctorialAction, MonadicAction,
    -- ** Atomic
    Atomic (try),
    -- ** Returnable
    Returnable (trivial),
    -- ** Loop
    Loop0, Loop1, count0, count1, repetition0, repetition1,
    -- ** Contramap
    ContravariantAction (..),
    -- ** Loss of movement
    LossOfMovement,
    -- ** Fallible
    Fallible (..),
    -- ** ...
    module Step.ActionTypes.GeneralActions,
  )
  where

import Step.ActionTypes.Atomic
import Step.ActionTypes.Constructors
import Step.ActionTypes.GeneralActions
import Step.ActionTypes.Join
import Step.ActionTypes.Loop
import Step.ActionTypes.Subtyping

{- $types

+--------------+----------+------------+------------+
|              | Succeeds | Advances   | Advances   |
|              |          | on success | on failure |
+--------------+----------+------------+------------+
| 'Move'       |          | Yes        |            |
+--------------+----------+------------+------------+
| 'Query'      |          | No         | No         |
+--------------+----------+------------+------------+
| 'Atom'       |          |            | No         |
+--------------+----------+------------+------------+
| 'AtomicMove' |          | Yes        | No         |
+--------------+----------+------------+------------+
| 'Sure'       | Yes      |            |            |
+--------------+----------+------------+------------+
| 'SureQuery'  | Yes      | No         | No         |
+--------------+----------+------------+------------+
| 'Fail'       | No       | No         | No         |
+--------------+----------+------------+------------+
| 'Any'        |          |            |            |
+--------------+----------+------------+------------+

-}

{- $subtypes

Arrows in the graph below indicate permitted use of 'cast'. (Not pictured: 'Fail')

![Action subtyping graph](graphics/action-subtyping.svg)

-}
