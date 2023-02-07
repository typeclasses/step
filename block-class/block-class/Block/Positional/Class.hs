module Block.Positional.Class where

import Block.Positional.Types
import Block.Singleton.Class

import Integer (Positive)

class (Singleton xs) => Positional xs where

    length :: xs -> Positive

    split :: Amount -> xs -> Split xs

    take :: Amount -> xs -> Take xs

    drop :: Amount -> xs -> Drop xs
