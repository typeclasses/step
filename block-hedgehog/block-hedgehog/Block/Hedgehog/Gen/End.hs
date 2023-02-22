module Block.Hedgehog.Gen.End (end) where

import Hedgehog (Gen)
import Block.Class.End (End (..))

import qualified Hedgehog.Gen as Gen

end :: Gen End
end = Gen.element [Front, Back]
