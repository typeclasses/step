module Step.Action.Loop where

import Step.Action.CanBeStatic
import Step.Action.Kinds
import Step.Action.IsAction
import Step.Action.Join
import Step.Action.KindJoin
import Step.Action.Subtyping

-- | Loop0 k k' means that a repetition of 0 or more k actions results in a k' action.
class
    ( IsAction k, IsAction k', IsAction (k :> k')
    , ActionJoin k k'
    , CanBeStatic k'
    , Is (k :> k') k'
    ) =>
    Loop0 k k' | k -> k'

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
class
    ( IsAction k, IsAction k', IsAction (k :> k')
    , ActionJoin k k'
    , Is k k'
    , Is (k :> k') k'
    ) =>
    Loop1 k k' | k -> k'

-- Atomic actions loose their atomicity when sequenced 2 or more times

instance Loop1 Atom Any
instance Loop1 AtomicMove Move

-- All other kinds are preserved by sequencing

instance Loop1 Any Any
instance Loop1 Query Query
instance Loop1 Move Move
instance Loop1 Sure Sure
instance Loop1 SureQuery SureQuery
