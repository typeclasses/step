module Step.Package.End where

import Essentials
import Step.Action.Core
import Step.Package.Failure
import Step.Interface

import qualified Step.Do as P

import SupplyChain (order)

atEnd :: SureQuery c m r Bool
atEnd = SureQuery \_ -> ResettingSequenceJob $ order next
    <&> \case{ End -> True; _ -> False }

end :: forall c m r. Query c m r ()
end = atEnd P.>>= requireTrue
