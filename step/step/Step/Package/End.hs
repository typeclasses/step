module Step.Package.End where

import Step.Action.Core
import Step.Error
import Step.Package.Failure
import Step.Interface

import qualified Step.Do as P
import qualified Step.Interface as Interface

import Data.Bool (Bool (..))
import Data.Function
import Data.Functor
import Data.Maybe
import SupplyChain (order)

import qualified SupplyChain

atEnd :: SureQuery c m e Bool
atEnd = SureQuery $ ResettingSequence $ order nextMaybe <&> isNothing

end :: forall c m e. ErrorContext e m => Query c m e ()
end = atEnd P.>>= requireTrue
