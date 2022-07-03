module Step.Action.Join where

import Step.Action.Family

import Step.Action.Kind (ActionKind)
import qualified Step.Action.Kind as Kind

class Join
    (k1 :: ActionKind)
    (k2 :: ActionKind)
    (k3 :: ActionKind)
      | k1 k2 -> k3
  where
    join ::
        Action k1 config cursor error m (Action k2 config cursor error m a)
        -> Action k3 config cursor error m a

-- todo: ugh there will be 64 of these
instance Join 'Kind.MoveUndo 'Kind.SureStatic 'Kind.Move where
instance Join 'Kind.SureStatic 'Kind.MoveUndo 'Kind.Move where
instance Join 'Kind.MoveUndo 'Kind.Move 'Kind.Move where
instance Join 'Kind.Move 'Kind.MoveUndo 'Kind.Move where
instance Join 'Kind.Move 'Kind.Move 'Kind.Move where
instance Join 'Kind.MoveUndo 'Kind.Any 'Kind.Move where
instance Join 'Kind.Any 'Kind.MoveUndo 'Kind.Move where
instance Join 'Kind.Move 'Kind.Any 'Kind.Move where
instance Join 'Kind.Any 'Kind.Move 'Kind.Move where
instance Join 'Kind.Any 'Kind.Any 'Kind.Any where
instance Join 'Kind.Any 'Kind.SureStatic 'Kind.Any where
instance Join 'Kind.SureStatic 'Kind.Any 'Kind.Any where
instance Join 'Kind.MoveUndo 'Kind.MoveUndo 'Kind.Move where
instance Join 'Kind.Move 'Kind.Undo 'Kind.Move where
instance Join 'Kind.Undo 'Kind.Move 'Kind.Move where
instance Join 'Kind.Move 'Kind.Static 'Kind.Move where
instance Join 'Kind.Static 'Kind.Move 'Kind.Move where
