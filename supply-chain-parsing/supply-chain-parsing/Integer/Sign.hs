module Integer.Sign where

import Prelude (Eq, Ord, (==))

data Sign = MinusSign | PlusSign
    deriving (Eq, Ord)

negate :: Sign -> Sign
negate PlusSign = MinusSign
negate MinusSign = PlusSign

multiply :: Sign -> Sign -> Sign
multiply a b = if a == b then PlusSign else MinusSign
