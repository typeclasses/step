module Step.Action.AlwaysMoves where

import Step.Action.Kinds

class AlwaysMoves (k :: ActionKind)
instance AlwaysMoves Move
instance AlwaysMoves MoveUndo
instance AlwaysMoves SureMove
