module Step.Action.KindJoin where

import Step.Action.Kinds

type a :> b = KindJoin a b

-- todo: there need to be all 64 instances
type family KindJoin (k1 :: ActionKind) (k2 :: ActionKind) :: ActionKind

-- Some of the action kinds compose very nicely with themselves.

type instance KindJoin Any       Any       = Any
type instance KindJoin Move      Move      = Move
type instance KindJoin Query     Query     = Query
type instance KindJoin Sure      Sure      = Sure
type instance KindJoin SureQuery SureQuery = SureQuery
type instance KindJoin SureMove  SureMove  = SureMove

-- Atomic, however, lose their atomicity when put in sequence.

type instance KindJoin MoveAtom  MoveAtom  = Move
type instance KindJoin Atom      Atom      = Any

-- SureQuery is easy: It never changes the kind of whatever it's joined with.

type instance KindJoin Any       SureQuery = Any
type instance KindJoin SureQuery Any       = Any

type instance KindJoin Query     SureQuery = Query
type instance KindJoin SureQuery Query     = Query

type instance KindJoin Move      SureQuery = Move
type instance KindJoin SureQuery Move      = Move

type instance KindJoin Atom      SureQuery = Atom
type instance KindJoin SureQuery Atom      = Atom

type instance KindJoin MoveAtom  SureQuery = MoveAtom
type instance KindJoin SureQuery MoveAtom  = MoveAtom

type instance KindJoin Sure      SureQuery = Sure
type instance KindJoin SureQuery Sure      = Sure

type instance KindJoin SureMove  SureQuery = SureMove
type instance KindJoin SureQuery SureMove  = SureMove

-- ...

type instance KindJoin Sure      SureMove  = Sure
type instance KindJoin SureMove  Sure      = Sure
type instance KindJoin MoveAtom  Move      = Move
type instance KindJoin Move      MoveAtom  = Move
type instance KindJoin MoveAtom  Any       = Move
type instance KindJoin Any       MoveAtom  = Move
type instance KindJoin Move      Any       = Move
type instance KindJoin Any       Move      = Move
type instance KindJoin Move      Atom      = Move
type instance KindJoin Atom      Move      = Move
type instance KindJoin Move      Query     = Move
type instance KindJoin Query     Move      = Move
type instance KindJoin Atom      Any       = Any
type instance KindJoin Any       Atom      = Any
type instance KindJoin MoveAtom  Sure      = Move
type instance KindJoin Sure      MoveAtom  = Move
