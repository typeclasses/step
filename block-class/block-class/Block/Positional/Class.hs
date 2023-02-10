module Block.Positional.Class
  (
    Positional (..),
  )
  where

import Essentials

import Block.Positional.Types
import Block.Singleton.Class

import Integer (Positive)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty, reverse)

import qualified Integer
import qualified Integer.Positive as Positive
import qualified Data.List.NonEmpty as NonEmpty

class (Singleton xs) => Positional xs where

    length :: xs -> Positive

    split :: Amount -> xs -> Split xs

    take :: Amount -> xs -> Take xs

    drop :: Amount -> xs -> Drop xs

instance Positional (NonEmpty xs) where

    length = Positive.length
