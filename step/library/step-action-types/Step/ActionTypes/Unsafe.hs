{-# language Unsafe #-}

-- | The only properties guaranteed by construction are that /Sure/ always succeeds and /Fail/ never does. The rest of the properties are not enforced by constructors. This module is, therefore, unsafe.

module Step.ActionTypes.Unsafe
  (
    -- * Action types
    Any (..),
    Atom (..),
    AtomicMove (..),
    Fail (..),
    Move (..),
    Query (..),
    Sure (..),
    SureQuery (..),

    -- * Coercion
    CoerceAny (anyIsoUnsafe),
    toAny, fromAnyUnsafe,

    -- * Change base
    ChangeBase, changeBase,

  )
  where

import Step.ActionTypes.CoerceAny
import Step.ActionTypes.Constructors
import Step.ActionTypes.ChangeBase
