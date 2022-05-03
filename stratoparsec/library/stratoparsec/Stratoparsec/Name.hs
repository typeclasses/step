module Stratoparsec.Name where

newtype Name = Name Text

deriving newtype instance IsString Name
