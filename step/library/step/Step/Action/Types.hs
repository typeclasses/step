{-# language Trustworthy #-}

-- | Abstract types whose unsafe constructors may be found in "Step.Action.Constructors"

module Step.Action.Types
  (
    ActionKind,
    Any,
    Query,
    Move,
    Atom,
    AtomicMove,
    Sure,
    SureQuery,
  )
  where

import Step.Action.Constructors
