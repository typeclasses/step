module Block.Class.Concat.Utilities where

import Block.Class.Concat.Class

import Data.Function (flip)
import Block.Class.End (End (..))

prepend :: Concat xs => End -> xs -> (xs -> xs)
prepend Front = (++)
prepend Back = flip (++)
