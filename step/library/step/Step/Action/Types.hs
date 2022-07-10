{-# language Trustworthy #-}

-- | Abstract types whose unsafe constructors may be found in "Step.Action.Constructors"

module Step.Action.Types
  (
    ActionKind,
    -- * The types
    Any,
    Query,
    Move,
    Atom,
    AtomicMove,
    Sure,
    SureQuery,
    Fail,
  )
  where

import Step.Action.Constructors
