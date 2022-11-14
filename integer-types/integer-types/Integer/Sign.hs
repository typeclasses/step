{-# language Safe #-}

module Integer.Sign
  (
    {- * Type -} Sign (..),
    {- * Operations -} negate, multiply,
  )
  where

import Prelude (Eq, Ord, (==), Show)

data Sign = MinusSign | PlusSign
    deriving (Eq, Ord, Show)

negate :: Sign -> Sign
negate PlusSign = MinusSign
negate MinusSign = PlusSign

multiply :: Sign -> Sign -> Sign
multiply a b = if a == b then PlusSign else MinusSign
