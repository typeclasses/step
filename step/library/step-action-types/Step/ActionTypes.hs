{-# language ExplicitNamespaces, Safe #-}

module Step.ActionTypes
  (
    -- * The types
    ActionKind,
    -- $types
    Any, Query, Move, Atom, AtomicMove,
    Sure, SureQuery, Fail,
    -- * Subtype relationships
    -- $subtypes
    Is, cast,
    -- * Monadic-style join
    Join, join, type (>>),
    -- * Miscellaneous classes
    -- ** Functorial, Monadic
    FunctorialAction, MonadicAction,
    -- ** Configurable
    Configurable (configure),
    -- ** Atomic
    Atomic (try),
    -- ** Returnable
    Returnable (trivial),
    -- ** Loop
    Loop0, Loop1,
  )
  where

import Step.ActionTypes.Atomic
import Step.ActionTypes.Returnable
import Step.ActionTypes.Config
import Step.ActionTypes.Functorial
import Step.ActionTypes.Join
import Step.ActionTypes.KindJoin
import Step.ActionTypes.Loop
import Step.ActionTypes.Monadic
import Step.ActionTypes.Subtyping
import Step.ActionTypes.Types

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
