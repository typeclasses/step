module Step.Action.Loop where

import Step.Action.Kinds
import Step.Action.UnifiedType
import Step.Action.Join
import Step.Action.KindJoin
import Step.Action.Lift

-- | Loop0 k k' means that a repetition of 0 or more k actions results in a k' action.
class
    ( IsAction k, IsAction k', IsAction (k :> k')
    , ActionJoin k k'
    , CanBeStatic k'
    , ActionLift (k :> k') k'
    ) =>
    Loop0 k k' | k -> k'

-- Backtracking actions loose their backtracking property when sequenced 2 or more times; guaranteed advancement is lost when sequencing 0 times

instance Loop0 Undo Any
instance Loop0 MoveUndo Any
instance Loop0 Move Any
instance Loop0 SureMove Sure

-- Other kinds are preserved

instance Loop0 Any Any
instance Loop0 Sure Sure
instance Loop0 SureStatic SureStatic
instance Loop0 Static Static

---

-- | Loop1 k k' means that a repetition of 1 or more k actions results in a k' action.
class
    ( IsAction k, IsAction k', IsAction (k :> k')
    , ActionJoin k k'
    , ActionLift k k'
    , ActionLift (k :> k') k'
    ) =>
    Loop1 k k' | k -> k'

-- Backtracking actions loose their backtracking property when sequenced 2 or more times

instance Loop1 Undo Any
instance Loop1 MoveUndo Move

-- All other kinds are preserved by sequencing

instance Loop1 Any Any
instance Loop1 Static Static
instance Loop1 Move Move
instance Loop1 Sure Sure
instance Loop1 SureStatic SureStatic
instance Loop1 SureMove SureMove
