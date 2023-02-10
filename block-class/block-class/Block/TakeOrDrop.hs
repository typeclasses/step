module Block.TakeOrDrop where

data TakeOrDrop = Take | Drop

opposite :: TakeOrDrop -> TakeOrDrop
opposite = \case Take -> Drop; Drop -> Take
