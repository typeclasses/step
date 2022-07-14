{-# language Trustworthy #-}

-- | Abstract types whose unsafe constructors may be found in "Step.ActionTypes.Unsafe"

module Step.ActionTypes.Types
  (
    Action,
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

import Step.ActionTypes.Constructors
