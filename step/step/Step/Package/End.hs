module Step.Package.End where

import Step.Action.Core
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

atEnd :: SureQuery c m r e Bool
atEnd = SureQuery \_ -> ResettingSequenceJob $ order nextMaybe <&> isNothing

end :: forall c m r. Query c m r r ()
end = atEnd P.>>= requireTrue
