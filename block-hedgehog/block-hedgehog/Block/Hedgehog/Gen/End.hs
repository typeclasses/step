module Block.Hedgehog.Gen.End (end) where

import Hedgehog (Gen)
import Block.End (End (..))

import qualified Hedgehog.Gen as Gen

{-| Shrinks toward 'Front' because implementing the front-side of
    an operation is usually easier than implementing the 'Back' -}
end :: Gen End
end = Gen.element [Front, Back]
