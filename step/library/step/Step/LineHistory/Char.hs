module Step.LineHistory.Char where

import Step.Internal.Prelude

import qualified Char

class Eq a => Char a
  where
    carriageReturn :: a
    lineFeed :: a

instance Char Char.Char
  where
    carriageReturn = '\r'
    lineFeed = '\n'