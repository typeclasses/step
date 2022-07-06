module Step.Action.KindJoin where

import Step.Action.Kinds

type a :> b = KindJoin a b

-- todo: there need to be all 64 instances
type family KindJoin (k1 :: ActionKind) (k2 :: ActionKind) :: ActionKind

-- Some of the action kinds compose very nicely with themselves.

type instance KindJoin Any        Any        = Any
type instance KindJoin Move       Move       = Move
type instance KindJoin Static     Static     = Static
type instance KindJoin Sure       Sure       = Sure
type instance KindJoin SureStatic SureStatic = SureStatic
type instance KindJoin SureMove   SureMove   = SureMove

-- Backtracking kinds, however, lose their backtracking property when put in sequence.

type instance KindJoin MoveAtom   MoveAtom   = Move
type instance KindJoin Atom       Atom       = Any

-- SureStatic is easy: It never changes the kind of whatever it's joined with.

type instance KindJoin Any        SureStatic = Any
type instance KindJoin SureStatic Any        = Any

type instance KindJoin Static     SureStatic = Static
type instance KindJoin SureStatic Static     = Static

type instance KindJoin Move       SureStatic = Move
type instance KindJoin SureStatic Move       = Move

type instance KindJoin Atom       SureStatic = Atom
type instance KindJoin SureStatic Atom       = Atom

type instance KindJoin MoveAtom   SureStatic = MoveAtom
type instance KindJoin SureStatic MoveAtom   = MoveAtom

type instance KindJoin Sure       SureStatic = Sure
type instance KindJoin SureStatic Sure       = Sure

type instance KindJoin SureMove   SureStatic = SureMove
type instance KindJoin SureStatic SureMove   = SureMove

-- ...

type instance KindJoin Sure       SureMove   = Sure
type instance KindJoin SureMove   Sure       = Sure
type instance KindJoin MoveAtom   Move       = Move
type instance KindJoin Move       MoveAtom   = Move
type instance KindJoin MoveAtom   Any        = Move
type instance KindJoin Any        MoveAtom   = Move
type instance KindJoin Move       Any        = Move
type instance KindJoin Any        Move       = Move
type instance KindJoin Move       Atom       = Move
type instance KindJoin Atom       Move       = Move
type instance KindJoin Move       Static     = Move
type instance KindJoin Static     Move       = Move
type instance KindJoin Atom       Any        = Any
type instance KindJoin Any        Atom       = Any
type instance KindJoin MoveAtom   Sure       = Move
type instance KindJoin Sure       MoveAtom   = Move
