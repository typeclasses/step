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

type instance KindJoin MoveUndo   MoveUndo   = Move
type instance KindJoin Undo       Undo       = Any

-- SureStatic is easy: It never changes the kind of whatever it's joined with.

type instance KindJoin Any        SureStatic = Any
type instance KindJoin SureStatic Any        = Any

type instance KindJoin Static     SureStatic = Static
type instance KindJoin SureStatic Static     = Static

type instance KindJoin Move       SureStatic = Move
type instance KindJoin SureStatic Move       = Move

type instance KindJoin Undo       SureStatic = Undo
type instance KindJoin SureStatic Undo       = Undo

type instance KindJoin MoveUndo   SureStatic = MoveUndo
type instance KindJoin SureStatic MoveUndo   = MoveUndo

type instance KindJoin Sure       SureStatic = Sure
type instance KindJoin SureStatic Sure       = Sure

type instance KindJoin SureMove   SureStatic = SureMove
type instance KindJoin SureStatic SureMove   = SureMove

-- ...

type instance KindJoin Sure       SureMove   = Sure
type instance KindJoin SureMove   Sure       = Sure
type instance KindJoin MoveUndo   Move       = Move
type instance KindJoin Move       MoveUndo   = Move
type instance KindJoin MoveUndo   Any        = Move
type instance KindJoin Any        MoveUndo   = Move
type instance KindJoin Move       Any        = Move
type instance KindJoin Any        Move       = Move
type instance KindJoin Move       Undo       = Move
type instance KindJoin Undo       Move       = Move
type instance KindJoin Move       Static     = Move
type instance KindJoin Static     Move       = Move
