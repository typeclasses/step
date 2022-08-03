module Step.ContextStack where

import Step.Internal.Prelude

import Text (Text)
import Seq (Seq)

import Optics

newtype ContextStack = ContextStack (Seq Text)
    deriving stock (Eq, Show)

instance IsList ContextStack where
    type Item ContextStack = Text
    fromList = ContextStack . fromList
    toList (ContextStack xs) = toList xs

contextStackSeq :: Iso' ContextStack (Seq Text)
contextStackSeq = coerced
